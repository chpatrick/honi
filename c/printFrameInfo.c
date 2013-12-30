#include "OniCAPI.h"
#include "OniVersion.h"

#define STR_LIT(arg) #arg
#define OK(fun, args) s = fun args; if (s != ONI_STATUS_OK) { perror("ERROR: " STR_LIT(fun)); exit(1); }

int main(int argc, char const *argv[])
{
  printf("Make sure a camera is plugged in!\n");

  OniStatus s;

  OK( oniInitialize, (ONI_API_VERSION) );

  OniDeviceInfo* pDevices;
  int pNumDevices;
  OK( oniGetDeviceList, (&pDevices, &pNumDevices) );

  OniDeviceHandle device;
  OK( oniDeviceOpen, (pDevices[0].uri, &device) );

  OniStreamHandle pStream;
  OK( oniDeviceCreateStream, (device, ONI_SENSOR_DEPTH, &pStream) );
  OK( oniStreamStart, (pStream) );

  OniFrame* pFrame;
  OK( oniStreamReadFrame, (pStream, &pFrame) );

  printf("sensorType            %d\n",  pFrame->sensorType);
  printf("timestamp             %lu\n", pFrame->timestamp);
  printf("frameIndex            %d\n",  pFrame->frameIndex);
  printf("width                 %d\n",  pFrame->width);
  printf("height                %d\n",  pFrame->height);
  printf("videoMode.pixelFormat %d\n",  pFrame->videoMode.pixelFormat);
  printf("videoMode.resolutionX %d\n",  pFrame->videoMode.resolutionX);
  printf("videoMode.resolutionY %d\n",  pFrame->videoMode.resolutionY);
  printf("videoMode.fps         %d\n",  pFrame->videoMode.fps);
  printf("croppingEnabled       %d\n",  pFrame->croppingEnabled);
  printf("cropOriginX           %d\n",  pFrame->cropOriginX);
  printf("cropOriginY           %d\n",  pFrame->cropOriginY);
  printf("stride                %d\n",  pFrame->stride);

  oniShutdown();

  return 0;
}
