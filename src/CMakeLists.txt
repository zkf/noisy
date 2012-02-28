cmake_minimum_required(VERSION 2.8)
set(CMAKE_BUILD_TYPE Debug)
project(noisy)

#add_definitions(-std=c99)
add_executable(noisy lts.cpp main.cpp)
