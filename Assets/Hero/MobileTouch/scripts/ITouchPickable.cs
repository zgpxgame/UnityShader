using UnityEngine;

public interface ITouchPickable 
{

	/// <summary>
	/// 是否可以长按
	/// </summary>
	/// <returns><c>true</c>, if long tap was caned, <c>false</c> otherwise.</returns>
	bool CanLongTap();

	/// <summary>
	/// Gets the transform.
	/// </summary>
	/// <returns>The transform.</returns>
	Transform GetTransform();
	/// <summary>
	/// 点击的时候是否选中目标
	/// </summary>
	/// <returns><c>true</c>, if in pick was pointed, <c>false</c> otherwise.</returns>
	T GetPickComponent<T>() where T : MonoBehaviour;

	bool PointInPick();
	/// <summary>
	/// Drag this instance.
	/// </summary>
	void Drag(Vector3 pos);
	/// <summary>
	/// Select this instance.
	/// </summary>
	bool Select();
	/// <summary>
	/// Click this instance.
	/// </summary>
	void Click();
	
	/// <summary>
	/// 是否超出范围
	/// </summary>
	bool IsOutRange(Vector3 pos);
	
	/// <summary>
	/// 改变目标的位置
	/// </summary>
	void ChangeTouchPos(int index);
	
	/// <summary>
	/// 获取最近的点
	/// </summary>
	Vector3 GetClosestPoint(Vector3 pos);
	
}
