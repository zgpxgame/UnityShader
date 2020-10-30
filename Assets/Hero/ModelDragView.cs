using BitBenderGames;
using UnityEngine;

public class ModelDragView : MonoBehaviour
{
    public bool allowUserInput = true; // set this to false to prevent the user from dragging the view
    public float sensitivity = 30.0f;
    public float dragAcceleration = 40.0f;
    public float dragDeceleration = 15.0f;
    public bool reverseControls = false;
    public float idealRotationSmoothingSpeed = 7.0f; // set to 0 to disable smoothing when rotating toward ideal direction

    private TouchInputController touchInput;
    private Transform cachedTransform;
    private Vector2 angularVelocity = Vector2.zero;
    private Quaternion idealRotation;
    private bool useAngularVelocity = false;
    private bool isDragging = false;
    private Vector3 lastDragPos;
    private Vector3 deltaMove;

    void Awake()
    {
        cachedTransform = transform;
        touchInput = GetComponent<TouchInputController>();
        touchInput.OnDragStart += OnDragStart;
        touchInput.OnDragUpdate += OnDrag;
        touchInput.OnDragStop += OnDragStop;
    }

    void Start()
    {
        IdealRotation = cachedTransform.rotation;
    }

    void OnDragStart(Vector3 pos, bool isLongTap)
    {
        isDragging = true;
        lastDragPos = pos;
    }

    void OnDragStop(Vector3 dragStopPos, Vector3 dragFinalMomentum)
    {
        isDragging = false;
    }
    
    void OnDrag(Vector3 dragPosStart, Vector3 dragPosCurrent, Vector3 correctionOffset)
    {
        deltaMove = dragPosCurrent - lastDragPos;
        lastDragPos = dragPosCurrent;
    }

    private void Update()
    {
        UpdateDrag();
    }

    void UpdateDrag()
    {
        if (isDragging && allowUserInput)
            useAngularVelocity = true;

        if (useAngularVelocity)
        {
            Vector3 localAngles = transform.localEulerAngles;
            Vector2 idealAngularVelocity = Vector2.zero;

            float accel = dragDeceleration;

            if (isDragging)
            {
                idealAngularVelocity = sensitivity * new Vector2(deltaMove.x, deltaMove.y);
                accel = dragAcceleration;
            }

            angularVelocity = Vector2.Lerp(angularVelocity, idealAngularVelocity, Time.deltaTime * accel);
            Vector2 angularMove = Time.deltaTime * angularVelocity;

            if (reverseControls)
                angularMove = -angularMove;

            // yaw angle
            localAngles.y -= angularMove.x;

            // apply
            transform.localEulerAngles = localAngles;
        }
        else
        {
            if (idealRotationSmoothingSpeed > 0)
                cachedTransform.rotation = Quaternion.Slerp(cachedTransform.rotation, IdealRotation, Time.deltaTime * idealRotationSmoothingSpeed);
            else
                cachedTransform.rotation = idealRotation;
        }
    }

    public Quaternion IdealRotation
    {
        get { return idealRotation; }
        set
        {
            idealRotation = value;
            useAngularVelocity = false;
        }
    }

    // Point the camera at the target point
    public void LookAt(Vector3 pos)
    {
        IdealRotation = Quaternion.LookRotation(pos - cachedTransform.position);
    }
}