cmake_minimum_required(VERSION 3.5)
project(tutorial_exercises VERSION 0.1.0)

find_package(rclada_common REQUIRED)
ada_begin_package()

# TODO: add necessary packages as the become necessary for the Ada exercises
find_package(rosidl_generator_ada REQUIRED)
find_package(rclada REQUIRED)
find_package(rclada_tf2 REQUIRED)
find_package(tutorial_common REQUIRED)

# TODO: add interfaces so their messages are generated. NOTE: only one call
# to ada_import_interfaces is possible. Pass all packages to the same call.
#ada_import_interfaces(
#        )

ada_add_executables(
        exercises               # CMake target
        ${PROJECT_SOURCE_DIR}   # GPR is right in the root of the package
        bin

        client_async
        client_sync
        epuck_commander
        epuck_wander
        publisher_dynamic
        publisher_static
        server_example
        subscriber_dynamic
        subscriber_static
        turtlebot_vfh
        turtlebot_vfh_tf2
        turtlebot_wander
        turtlesim_commander
        turtlesim_hello
        turtlesim_mosaic)

ada_end_package()
