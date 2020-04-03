#include <stdio.h>
#include <stdlib.h>
#include <xcb/xcb_xrm.h>

char* get_resource_value(const char* resource) {
    xcb_connection_t* connection = NULL;
    xcb_xrm_database_t* db = NULL;
    char* value = NULL;

    connection = xcb_connect(NULL, NULL);
    if (xcb_connection_has_error(connection)) {
        goto DONE;
    }

    db = xcb_xrm_database_from_default(connection);
    if (!db) {
        goto DONE;
    }

    xcb_xrm_resource_get_string(db, resource, NULL, &value);

DONE:
    xcb_xrm_database_free(db);
    xcb_disconnect(connection);

    return value;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "xrquery: Expected 1 argument, got %d\n", argc - 1);
        return 1;
    }

    char* value = get_resource_value(argv[1]);

    if (!value) {
        return 1;
    }

    puts(value);
    free(value);

    return 0;
}
