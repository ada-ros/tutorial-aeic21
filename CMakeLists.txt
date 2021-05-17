cmake_minimum_required(VERSION 3.5)
project(tutorial_aeic21 VERSION 0.1.0)

find_package(rclada_common REQUIRED)
ada_begin_package()

find_package(geometry_msgs REQUIRED)
find_package(rosidl_generator_ada REQUIRED)
find_package(rclada REQUIRED)
find_package(rclada_tf2 REQUIRED)

ada_import_interfaces(geometry_msgs)

ada_add_executables(
        tut21_exercises # CMake target
        ${PROJECT_SOURCE_DIR}/gpr_exercises
        bin

        epuck_test)

ada_end_package()