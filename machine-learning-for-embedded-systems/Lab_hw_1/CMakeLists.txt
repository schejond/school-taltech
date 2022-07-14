cmake_minimum_required(VERSION 3.21)
project(Lab_hw_1)

set(CMAKE_C_STANDARD 11)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c11 -Wall -Wextra -lm")

include_directories(src)

add_executable(Lab_hw_1
        src/main.c
        src/simple_neural_networks.c
        src/simple_neural_networks.h)
target_link_libraries(Lab_hw_1 m)
