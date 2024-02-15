// Check if DeviceMotionEvent is supported by the browser
if ('DeviceMotionEvent' in window) {
    // Request permission for iOS 13+ devices
    if (typeof DeviceMotionEvent.requestPermission === 'function') {
        DeviceMotionEvent.requestPermission()
            .then(permissionState => {
                if (permissionState === 'granted') {
                    window.addEventListener('devicemotion', handleMotionEvent, true);
                } else {
                    console.log('Permission to access accelerometer was denied.');
                }
            })
            .catch(console.error);
    } else {
        // Non-iOS 13+ devices do not need permission
        window.addEventListener('devicemotion', handleMotionEvent, true);
    }
} else {
    console.log('DeviceMotionEvent is not supported by your browser.');
}

function handleMotionEvent(event) {
    const acceleration = event.acceleration;

    if (acceleration) {
        const { x, y, z } = acceleration;
        console.log(`Acceleration along the X-axis: ${x}`);
        console.log(`Acceleration along the Y-axis: ${y}`);
        console.log(`Acceleration along the Z-axis: ${z}`);
    } else {
        console.log('Acceleration data is not available.');
    }
}

console.log("accelerometer.js loaded successfully");
