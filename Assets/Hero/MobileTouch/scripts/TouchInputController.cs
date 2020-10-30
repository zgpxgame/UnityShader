// /************************************************************
// *                                                           *
// *   Mobile Touch Camera                                     *
// *                                                           *
// *   Created 2015 by BitBender Games                         *
// *                                                           *
// *   bitbendergames@gmail.com                                *
// *                                                           *
// ************************************************************/

using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using UnityEngine.Events;
using UnityEngine.EventSystems;

namespace BitBenderGames
{

	public class TouchInputController : MonoBehaviour
	{

		[Header("Expert Mode")]
		[SerializeField]
		private bool expertModeEnabled;
		[SerializeField]
		[Tooltip("当手指在一个物品上停留至少这个时间而不动时，这个手势就被认为是一个长敲击.")]
		private float clickDurationThreshold = 0.7f;
		[SerializeField]
		[Tooltip("当连续两次点击之间的时间短于此时间时，可以识别双击手势.")]
		private float doubleclickDurationThreshold = 0.5f;

		[SerializeField]
		[Tooltip("此值控制用户必须执行倾斜手势才能将其识别为垂直线的距离.")]
		private float tiltMoveDotTreshold = 0.7f;
		[SerializeField]
		[Tooltip("阈值，用于检测手指是否水平到足以启动倾斜。使用此值可以防止将垂直手指放置计算为倾斜手势.")]
		private float tiltHorizontalDotThreshold = 0.5f;
		[SerializeField]
		[Tooltip("当用户的手指移动的距离超过这个值时，就会开始拖动。该值被定义为规范化值。拖动屏幕的整个宽度等于1。拖动整个屏幕的高度也等于1")]
		private float dragStartDistanceThresholdRelative = 0.05f;
		[SerializeField]
		[Tooltip("当启用此标志时，当长点击时间成功时，立即调用拖动启动事件.")]
		private bool longTapStartsDrag = false;

		[SerializeField]
		private bool checkInputOnUI = false;

		private float lastFingerDownTimeReal;
		private float lastClickTimeReal;
		private bool wasFingerDownLastFrame;
		private Vector3 lastFinger0DownPos;


		private const float dragDurationThreshold = 0.01f;

		private bool isDragging;
		private Vector3 dragStartPos;
		private Vector3 dragStartOffset;

		private List<Vector3> DragFinalMomentumVector { get; set; }
		private const int momentumSamplesCount = 5;

		private float pinchStartDistance;
		private List<Vector3> pinchStartPositions;
		private List<Vector3> touchPositionLastFrame;
		private Vector3 pinchRotationVectorStart = Vector3.zero;
		private Vector3 pinchVectorLastFrame = Vector3.zero;
		private float totalFingerMovement;

		private bool wasDraggingLastFrame;
		private bool wasPinchingLastFrame;

		private bool isPinching;

		private bool isInputOnLockedArea = false;

		private float timeSinceDragStart = 0;

		public delegate void InputDragStartDelegate(Vector3 pos, bool isLongTap);
		public delegate void Input1PositionDelegate(Vector3 pos);

		public event InputDragStartDelegate OnDragStart;
		public event Input1PositionDelegate OnFingerDown;
		public event System.Action OnFingerUp;

		public delegate void DragUpdateDelegate(Vector3 dragPosStart, Vector3 dragPosCurrent, Vector3 correctionOffset);
		public event DragUpdateDelegate OnDragUpdate;

		public delegate void DragStopDelegate(Vector3 dragStopPos, Vector3 dragFinalMomentum);
		public event DragStopDelegate OnDragStop;

		public delegate void PinchStartDelegate(Vector3 pinchCenter, float pinchDistance);
		public event PinchStartDelegate OnPinchStart;

		public delegate void PinchUpdateDelegate(Vector3 pinchCenter, float pinchDistance, float pinchStartDistance);
		public event PinchUpdateDelegate OnPinchUpdate;

		public delegate void PinchUpdateExtendedDelegate(PinchUpdateData pinchUpdateData);
		public event PinchUpdateExtendedDelegate OnPinchUpdateExtended;

		public event System.Action OnPinchStop;

		public delegate void InputLongTapProgress(float progress);
		public event InputLongTapProgress OnLongTapProgress;

		private bool isClickPrevented;

		public delegate void InputClickDelegate(Vector3 clickPosition, bool isDoubleClick, bool isLongTap);
		public event InputClickDelegate OnInputClick;

		private bool isFingerDown;

		public bool LongTapStartsDrag { get { return longTapStartsDrag; } }

		public bool IsInputOnLockedArea
		{
			get { return isInputOnLockedArea; }
			set { isInputOnLockedArea = value; }
		}

		//
		public void Awake()
		{
			lastFingerDownTimeReal = 0;
			lastClickTimeReal = 0;
			lastFinger0DownPos = Vector3.zero;
			dragStartPos = Vector3.zero;
			isDragging = false;
			wasFingerDownLastFrame = false;
			DragFinalMomentumVector = new List<Vector3>();
			pinchStartPositions = new List<Vector3>() { Vector3.zero, Vector3.zero };
			touchPositionLastFrame = new List<Vector3>() { Vector3.zero, Vector3.zero };
			pinchStartDistance = 1;
			isPinching = false;
			isClickPrevented = false;
		}

		/// <summary>
		/// 禁止拖拽，并且这个可以做其他的事情，
		/// </summary>
		/// <param name="go">Go.</param>
		public void OnEventTriggerPointerDown(GameObject go)
		{
			isInputOnLockedArea = true;
		}

		public void OnEventTriggerPointerDown(BaseEventData baseEventData)
		{
			isInputOnLockedArea = true;
		}


		private void InputDonwOnUI()
		{
			if (isInputOnLockedArea == true)
			{
				return;
			}

			if (Application.isMobilePlatform)
			{
				if (Input.touchCount > 0 && EventSystem.current.IsPointerOverGameObject(Input.GetTouch(0).fingerId))
				{
					isInputOnLockedArea = true;
				}
			}
			//TODO PC端
			else
			{
				if (isFingerDown == false && TouchWrapper.IsFingerDown == true)
				{
					if (EventSystem.current.IsPointerOverGameObject())
					{
						isInputOnLockedArea = true;
					}
				}
			}

		}

		public void Update()
		{
			if (checkInputOnUI)
			{
				InputDonwOnUI();
			}

			if (TouchWrapper.IsFingerDown == false)
			{
				isInputOnLockedArea = false;
			}

			bool pinchToDragCurrentFrame = false;

			if (isInputOnLockedArea == false)
			{

				#region pinch
				if (isPinching == false)
				{
					if (TouchWrapper.TouchCount == 2)
					{
						StartPinch();
						isPinching = true;
					}
				}
				else
				{
					if (TouchWrapper.TouchCount < 2)
					{
						StopPinch();
						isPinching = false;
					}
					else if (TouchWrapper.TouchCount == 2)
					{
						UpdatePinch();
					}
				}
				#endregion

				#region drag
				if (isPinching == false)
				{
					if (wasPinchingLastFrame == false)
					{
						if (wasFingerDownLastFrame == true && TouchWrapper.IsFingerDown)
						{
							if (isDragging == false)
							{
								float dragDistance = GetRelativeDragDistance(TouchWrapper.Touch0.Position, dragStartPos);
								float dragTime = Time.realtimeSinceStartup - lastFingerDownTimeReal;

								bool isLongTap = dragTime > clickDurationThreshold;
								if (OnLongTapProgress != null)
								{
									float longTapProgress = 0;
									if (Mathf.Approximately(clickDurationThreshold, 0) == false)
									{
										longTapProgress = Mathf.Clamp01(dragTime / clickDurationThreshold);
									}
									OnLongTapProgress(longTapProgress);
								}

								if ((dragDistance >= dragStartDistanceThresholdRelative && dragTime >= dragDurationThreshold)
								  || (longTapStartsDrag == true && isLongTap == true))
								{
									isDragging = true;
									dragStartOffset = lastFinger0DownPos - dragStartPos;
									dragStartPos = lastFinger0DownPos;
									DragStart(dragStartPos, isLongTap);
								}
							}
						}
					}
					else
					{
						if (TouchWrapper.IsFingerDown == true)
						{
							isDragging = true;
							dragStartPos = TouchWrapper.Touch0.Position;
							DragStart(dragStartPos, false);
							pinchToDragCurrentFrame = true;
						}
					}

					if (isDragging == true && TouchWrapper.IsFingerDown == true)
					{
						DragUpdate(TouchWrapper.Touch0.Position);
					}

					if (isDragging == true && TouchWrapper.IsFingerDown == false)
					{
						isDragging = false;
						DragStop(lastFinger0DownPos);
					}
				}
				#endregion

				#region click
				if (isPinching == false && isDragging == false && wasPinchingLastFrame == false && wasDraggingLastFrame == false && isClickPrevented == false)
				{
					if (wasFingerDownLastFrame == false && TouchWrapper.IsFingerDown)
					{
						lastFingerDownTimeReal = Time.realtimeSinceStartup;
						dragStartPos = TouchWrapper.Touch0.Position;
						FingerDown(TouchWrapper.AverageTouchPos);
					}

					if (wasFingerDownLastFrame == true && TouchWrapper.IsFingerDown == false)
					{
						float fingerDownUpDuration = Time.realtimeSinceStartup - lastFingerDownTimeReal;

						if (wasDraggingLastFrame == false && wasPinchingLastFrame == false)
						{
							float clickDuration = Time.realtimeSinceStartup - lastClickTimeReal;

							bool isDoubleClick = clickDuration < doubleclickDurationThreshold;
							bool isLongTap = fingerDownUpDuration > clickDurationThreshold;

							if (OnInputClick != null)
							{
								OnInputClick.Invoke(lastFinger0DownPos, isDoubleClick, isLongTap);
							}

							lastClickTimeReal = Time.realtimeSinceStartup;
						}

					}
				}
				#endregion
			}

			if (isDragging && TouchWrapper.IsFingerDown && pinchToDragCurrentFrame == false)
			{
				DragFinalMomentumVector.Add(TouchWrapper.Touch0.Position - lastFinger0DownPos);
				if (DragFinalMomentumVector.Count > momentumSamplesCount)
				{
					DragFinalMomentumVector.RemoveAt(0);
				}
			}

			if (isInputOnLockedArea == false)
			{
				wasFingerDownLastFrame = TouchWrapper.IsFingerDown;
			}
			if (wasFingerDownLastFrame == true)
			{
				lastFinger0DownPos = TouchWrapper.Touch0.Position;
			}

			wasDraggingLastFrame = isDragging;
			wasPinchingLastFrame = isPinching;

			if (TouchWrapper.TouchCount == 0)
			{
				isClickPrevented = false;
				if (isFingerDown == true)
				{
					FingerUp();
				}
			}
		}

		public void RestartDrag()
		{
			if (!isDragging)
				return;
			
			DragStop(lastFinger0DownPos);

			if (TouchWrapper.TouchCount > 0)
			{
				dragStartOffset = Vector3.zero;
				dragStartPos = TouchWrapper.Touch0.Position;
				DragStart(dragStartPos, false);
			}
		}

		private void StartPinch()
		{
			pinchStartPositions[0] = touchPositionLastFrame[0] = TouchWrapper.Touches[0].Position;
			pinchStartPositions[1] = touchPositionLastFrame[1] = TouchWrapper.Touches[1].Position;

			pinchStartDistance = GetPinchDistance(pinchStartPositions[0], pinchStartPositions[1]);
			if (OnPinchStart != null)
			{
				OnPinchStart.Invoke((pinchStartPositions[0] + pinchStartPositions[1]) * 0.5f, pinchStartDistance);
			}
			isClickPrevented = true;
			pinchRotationVectorStart = TouchWrapper.Touches[1].Position - TouchWrapper.Touches[0].Position;
			pinchVectorLastFrame = pinchRotationVectorStart;
			totalFingerMovement = 0;
		}

		private void UpdatePinch()
		{
			float pinchDistance = GetPinchDistance(TouchWrapper.Touches[0].Position, TouchWrapper.Touches[1].Position);
			Vector3 pinchVector = TouchWrapper.Touches[1].Position - TouchWrapper.Touches[0].Position;
			float pinchAngleSign = Vector3.Cross(pinchVectorLastFrame, pinchVector).z < 0 ? -1 : 1;
			float pinchAngleDelta = 0;
			if (Mathf.Approximately(Vector3.Distance(pinchVectorLastFrame, pinchVector), 0) == false)
			{
				pinchAngleDelta = Vector3.Angle(pinchVectorLastFrame, pinchVector) * pinchAngleSign;
			}
			float pinchVectorDeltaMag = Mathf.Abs(pinchVectorLastFrame.magnitude - pinchVector.magnitude);
			float pinchAngleDeltaNormalized = 0;
			if (Mathf.Approximately(pinchVectorDeltaMag, 0) == false)
			{
				pinchAngleDeltaNormalized = pinchAngleDelta / pinchVectorDeltaMag;
			}
			Vector3 pinchCenter = (TouchWrapper.Touches[0].Position + TouchWrapper.Touches[1].Position) * 0.5f;

			#region tilting gesture
			float pinchTiltDelta = 0;
			Vector3 touch0DeltaRelative = GetTouchPositionRelative(TouchWrapper.Touches[0].Position - touchPositionLastFrame[0]);
			Vector3 touch1DeltaRelative = GetTouchPositionRelative(TouchWrapper.Touches[1].Position - touchPositionLastFrame[1]);
			float touch0DotUp = Vector2.Dot(touch0DeltaRelative.normalized, Vector2.up);
			float touch1DotUp = Vector2.Dot(touch1DeltaRelative.normalized, Vector2.up);
			float pinchVectorDotHorizontal = Vector3.Dot(pinchVector.normalized, Vector3.right);
			if (Mathf.Sign(touch0DotUp) == Mathf.Sign(touch1DotUp))
			{
				if (Mathf.Abs(touch0DotUp) > tiltMoveDotTreshold && Mathf.Abs(touch1DotUp) > tiltMoveDotTreshold)
				{
					if (Mathf.Abs(pinchVectorDotHorizontal) >= tiltHorizontalDotThreshold)
					{
						pinchTiltDelta = 0.5f * (touch0DeltaRelative.y + touch1DeltaRelative.y);
					}
				}
			}
			totalFingerMovement += touch0DeltaRelative.magnitude + touch1DeltaRelative.magnitude;
			#endregion

			if (OnPinchUpdate != null)
			{
				OnPinchUpdate.Invoke(pinchCenter, pinchDistance, pinchStartDistance);
			}
			if (OnPinchUpdateExtended != null)
			{
				OnPinchUpdateExtended(new PinchUpdateData() { pinchCenter = pinchCenter, pinchDistance = pinchDistance, pinchStartDistance = pinchStartDistance, pinchAngleDelta = pinchAngleDelta, pinchAngleDeltaNormalized = pinchAngleDeltaNormalized, pinchTiltDelta = pinchTiltDelta, pinchTotalFingerMovement = totalFingerMovement });
			}
			pinchVectorLastFrame = pinchVector;
			touchPositionLastFrame[0] = TouchWrapper.Touches[0].Position;
			touchPositionLastFrame[1] = TouchWrapper.Touches[1].Position;

		}

		private float GetPinchDistance(Vector3 pos0, Vector3 pos1)
		{
			float distanceX = Mathf.Abs(pos0.x - pos1.x) / Screen.width;
			float distanceY = Mathf.Abs(pos0.y - pos1.y) / Screen.height;
			return (Mathf.Sqrt(distanceX * distanceX + distanceY * distanceY));
		}

		private void StopPinch()
		{
			dragStartOffset = Vector3.zero;
			if (OnPinchStop != null)
			{
				OnPinchStop.Invoke();
			}
		}

		private void DragStart(Vector3 pos, bool isLongTap)
		{
			if (OnDragStart != null)
			{
				OnDragStart(pos, isLongTap);
			}
			isClickPrevented = true;
			timeSinceDragStart = 0;
			DragFinalMomentumVector.Clear();
		}

		private void DragUpdate(Vector3 pos)
		{
			if (OnDragUpdate != null)
			{
				timeSinceDragStart += Time.deltaTime;
				Vector3 offset = Vector3.Lerp(Vector3.zero, dragStartOffset, Mathf.Clamp01(timeSinceDragStart * 10.0f));
				OnDragUpdate(dragStartPos, pos, offset);
			}
		}

		private void DragStop(Vector3 pos)
		{

			if (OnDragStop != null)
			{
				Vector3 momentum = Vector3.zero;
				if (DragFinalMomentumVector.Count > 0)
				{
					for (int i = 0; i < DragFinalMomentumVector.Count; ++i)
					{
						momentum += DragFinalMomentumVector[i];
					}
					momentum /= DragFinalMomentumVector.Count;
				}
				OnDragStop(pos, momentum);
			}

			DragFinalMomentumVector.Clear();
		}

		private void FingerDown(Vector3 pos)
		{
			isFingerDown = true;
			if (OnFingerDown != null)
			{
				OnFingerDown(pos);
			}
		}

		private void FingerUp()
		{
			isFingerDown = false;
			if (OnFingerUp != null)
			{
				OnFingerUp();
			}
		}

		private Vector3 GetTouchPositionRelative(Vector3 touchPosScreen)
		{
			return (new Vector3(touchPosScreen.x / (float)Screen.width, touchPosScreen.y / (float)Screen.height, touchPosScreen.z));
		}

		private float GetRelativeDragDistance(Vector3 pos0, Vector3 pos1)
		{
			Vector2 dragVector = pos0 - pos1;
			float dragDistance = new Vector2(dragVector.x / Screen.width, dragVector.y / Screen.height).magnitude;
			return dragDistance;
		}
	}
}
