cmake_minimum_required(VERSION 3.5)
project(tutorial_common)

# find dependencies
find_package(ament_cmake REQUIRED)
find_package(rclada_common REQUIRED)
find_package(rclada REQUIRED)
find_package(rclada_tf2 REQUIRED)
find_package(rosidl_generator_ada REQUIRED)
find_package(sensor_msgs REQUIRED)

ada_begin_package()

ada_import_interfaces(sensor_msgs)

ada_add_library(
  tutorial_common_lib
  ${PROJECT_SOURCE_DIR}
  tutorial_common.gpr
)

ada_end_package()