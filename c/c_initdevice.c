#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>


#include "XInput.h"

//
// Globals.
//

// Holds the values for the Wacom API.
xinput_values_t g_xinput_values = {
  .isWacom = true,
};

// Holds XInput wrapper state.
xinput_state_t g_xinput_state = {
  .refcount = 0,

  .thread = 0,

  .screen_width = 0,
  .screen_height = 0,

  // Valuator ranges.
  .absx_min = 0,
  .absx_min = 0,
  .absy_max = 1,
  .absy_max = 1,
  .pressure_min = 0,
  .pressure_max = 1,
  .tilt_min = 0,
  .tilt_max = 1,

  // XInput event types.
  .motionType = -1,
  .buttonPressType = -1,
  .buttonReleaseType = -1,
  .proximityInType = -1,
  .proximityOutType = -1,
};

//
// Functions.
//

static bool xinput_findDevices(Display *display, XDeviceInfo *stylus_info, XDeviceInfo *eraser_info)
{
  int found = 0;

  int num;
  XDeviceInfo *devices = XListInputDevices(display, &num);

  Atom stylus = XInternAtom(display, "STYLUS", false);
  Atom eraser = XInternAtom(display, "ERASER", false);

  for (int i = 0; i < num; i++) {
    debug("Got device \"%s\"\n", devices[i].name);

    if (devices[i].type == stylus) {
      found++;

      // Fill tablet model here.
      snprintf(g_xinput_values.tabletModel, sizeof(g_xinput_values.tabletModel), "%s", devices[i].name);
      snprintf(g_xinput_values.tabletModelID, sizeof(g_xinput_values.tabletModelID), "%s", devices[i].name);

      debug("Found stylus.\n");

      memcpy(stylus_info, &devices[i], sizeof(XDeviceInfo));

      XAnyClassPtr any = (XAnyClassPtr)devices[i].inputclassinfo;

      for (int j = 0; j < devices[i].num_classes; j++) {
        if (any->class == ValuatorClass) {
          XValuatorInfoPtr v = (XValuatorInfoPtr)any;

          // X.
          g_xinput_state.absx_min = v->axes[VALUATOR_ABSX].min_value;
          g_xinput_state.absx_min = v->axes[VALUATOR_ABSX].max_value;
          debug("X valuator range: min=%d, max=%d\n", v->axes[VALUATOR_ABSX].min_value, v->axes[VALUATOR_ABSX].max_value);

          // Y.
          g_xinput_state.absy_min = v->axes[VALUATOR_ABSY].min_value;
          g_xinput_state.absy_min = v->axes[VALUATOR_ABSY].max_value;
          debug("Y valuator range: min=%d, max=%d\n", v->axes[VALUATOR_ABSY].min_value, v->axes[VALUATOR_ABSY].max_value);

          // Pressure.
          g_xinput_state.pressure_min = v->axes[VALUATOR_PRESSURE].min_value;
          g_xinput_state.pressure_max = v->axes[VALUATOR_PRESSURE].max_value;
          debug("Pressure valuator range: min=%d, max=%d\n", v->axes[VALUATOR_PRESSURE].min_value, v->axes[VALUATOR_PRESSURE].max_value);

          // Tilt. Assuming that the range is the same for X and Y...
          g_xinput_state.tilt_min = v->axes[VALUATOR_TILTX].min_value;
          g_xinput_state.tilt_max = v->axes[VALUATOR_TILTX].max_value;
          debug("Tilt valuator range: min=%d, max=%d\n", v->axes[VALUATOR_TILTX].min_value, v->axes[VALUATOR_TILTX].max_value);

          break;
        }

        any = (XAnyClassPtr)((char *)any + any->length);
      }
    } else if (devices[i].type == eraser) {
      found++;

      debug("Found eraser.\n");

      memcpy(eraser_info, &devices[i], sizeof(XDeviceInfo));
    }
  }

  XFreeDeviceList(devices);

  return (found == 2);
}


void find_wacom( char* stylus_name, char* eraser_name) 
{
  Display *display = XOpenDisplay(NULL); 
  assert(display != NULL);

  g_xinput_state.screen_width = XWidthOfScreen(DefaultScreenOfDisplay(display));
  g_xinput_state.screen_height = XHeightOfScreen(DefaultScreenOfDisplay(display));

  XDeviceInfo stylus_info, eraser_info;

  if (!xinput_findDevices(display, &stylus_info, &eraser_info)) {
    fprintf(stderr, "Couldn't find device.\n");
  }
  else { 
    printf("found stylus=%s \n", stylus_info.name ); 
    printf("found eraser=%s \n", eraser_info.name ); 
    strcpy(stylus_name, stylus_info.name);
    strcpy(eraser_name, eraser_info.name); 
  }
  return ; 
}



void initdevice ( GtkWidget* widget
                , int* core
                , int* stylus
                , int* eraser
		, int* touch 
                , char* corepointername
                , char* stylusname
                , char* erasername
		, char* touchname
                )
{

 GdkDeviceManager *dev_manager = gdk_display_get_device_manager (gtk_widget_get_display (widget));
 
  printf("initdevice : stylusname = %s\n", stylusname );
  printf("initdevice : erasername = %s\n", erasername );
  printf("initdevice : touchname = %s\n", touchname );

  GList* dev_list;
  GdkDevice* device;
  dev_list = gdk_device_manager_list_devices(dev_manager,GDK_DEVICE_TYPE_SLAVE);
  (*stylus) = 0;
  while (dev_list != NULL) {
    // printf ("one device\n"); 
    device = (GdkDevice *)dev_list->data;
    // printf(" %d : %s \n", device, gdk_device_get_name (device) );
    if (device != gdk_device_manager_get_client_pointer(dev_manager)) {
      // #ifdef ENABLE_XINPUT_BUGFIX
      gdk_device_set_axis_use(device, 0, GDK_AXIS_IGNORE);
      gdk_device_set_axis_use(device, 1, GDK_AXIS_IGNORE);
      // #endif
      gdk_device_set_mode(device, GDK_MODE_SCREEN);

      // printf("This is xinput device %s \n", device -> name);
      if( !strcmp (gdk_device_get_name (device), stylusname) ) {
        // printf("got stylus\n");   
        (*stylus) = (int) device; 
      } 
      if( !strcmp (gdk_device_get_name (device), erasername) ) { 
        // printf("got eraser\n");
        (*eraser) = (int) device;
      } 
      if( !strcmp (gdk_device_get_name (device), touchname) ) { 
        // printf("got eraser\n");
        (*touch) = (int) device;
      } 
    } 
    else { 
      if( !strcmp (gdk_device_get_name (device), corepointername) ) { 
        // printf("got Core Pointer\n"); 
        (*core) = (int) device; 
      } 
    } 
    dev_list = dev_list->next; 
  }

}
