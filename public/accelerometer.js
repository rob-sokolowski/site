const initAccelerometer = async (app) => {
    const handleOrientationEvent = (event) => {
        const { alpha, beta, gamma } = event;

        console.log(`Rotation around Z-axis (alpha): ${alpha}`);
        // console.log(`Rotation around X-axis (beta): ${beta}`);
        // console.log(`Rotation around Y-axis (gamma): ${gamma}`);

        app.ports.markLoadVegaSequenceFinished.send(0);
    }

    // Check if DeviceOrientationEvent is supported by the browser
    if ('DeviceOrientationEvent' in window) {
        // Request permission for iOS 13+ devices
        if (typeof DeviceOrientationEvent.requestPermission === 'function') {
            DeviceOrientationEvent.requestPermission()
                .then(permissionState => {
                    if (permissionState === 'granted') {
                        window.addEventListener('deviceorientation', handleOrientationEvent, true);
                    } else {
                        console.log('Permission to access device orientation was denied.');
                    }
                })
                .catch(console.error);
        } else {
            // Non-iOS 13+ devices do not need permission
            window.addEventListener('deviceorientation', handleOrientationEvent, true);
        }
    } else {
        console.log('DeviceOrientationEvent is not supported by your browser.');
    }
};
