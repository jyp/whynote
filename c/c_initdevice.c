#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>


void initdevice ( GtkWidget* widget
                , int* stylus
                , int* eraser
		, int* touch 
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
    printf(" %d : %s \n", (int)device, gdk_device_get_name (device));
    {
      /* gdk_device_set_axis_use(device, 0, GDK_AXIS_IGNORE); */
      /* gdk_device_set_axis_use(device, 1, GDK_AXIS_IGNORE); */
      /* gdk_device_set_mode(device, GDK_MODE_SCREEN); */

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
    dev_list = dev_list->next; 
  }

}
