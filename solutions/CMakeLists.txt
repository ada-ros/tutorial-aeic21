cmake_minimum_required(VERSION 3.5)
project(tutorial_solutions VERSION 0.1.0)

find_package(rclada_common REQUIRED)
ada_begin_package()

find_package(geometry_msgs REQUIRED)
find_package(rosidl_generator_ada REQUIRED)
find_package(rclada REQUIRED)
find_package(rclada_tf2 REQUIRED)
find_package(sensor_msgs REQUIRED)
find_package(turtlesim REQUIRED)
find_package(tutorial_common REQUIRED)

ada_import_interfaces(
        geometry_msgs
        nav_msgs
        sensor_msgs
        turtlesim)

ada_add_executables(
        solutions               # CMake target
        ${PROJECT_SOURCE_DIR}   # Project is in the package root
        bin

        sol_client_async
        sol_client_sync
        sol_epuck_commander
        sol_epuck_wander
        sol_publisher_dynamic
        sol_publisher_static
        sol_server_example
        sol_subscriber_dynamic
        sol_subscriber_static
        sol_turtlebot_vfh
        sol_turtlebot_vfh_tf2
        sol_turtlebot_wander
        sol_turtlesim_commander
        sol_turtlesim_hello
        sol_turtlesim_mosaic)

ada_end_package()