/*
 ============================================================================
 Name        : Lab_hw_1.c
 Author      : Ondřej Schejbal
 Copyright   : TalTech
 Description : Home assignment 1
 ============================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include "simple_neural_networks.h"

// Size of the layers
#define NUM_OF_FEATURES   	3
#define NUM_OF_HID1_NODES	5
#define NUM_OF_HID2_NODES	4
#define NUM_OF_OUT_NODES	1
#define NUM_OF_INPUT_DATA   11
#define NUM_OF_EPOCHS       1000000
#define MIN_NUM_OF_EPOCHS   1000
#define ACCEPTED_COST_VALUE 0.0002

double learning_rate = 0.01;

/* Input layer to hidden layer */
double a1[NUM_OF_INPUT_DATA][NUM_OF_HID1_NODES];	// activation function
double b1[NUM_OF_HID1_NODES];		                // bias
double z1[NUM_OF_INPUT_DATA][NUM_OF_HID1_NODES];	// output vector

// Input layer to 1st hidden layer weight matrix
double w1[NUM_OF_HID1_NODES][NUM_OF_FEATURES];

// Hidden_layer_1 to Hidden_layer_2
double a2[NUM_OF_INPUT_DATA][NUM_OF_HID2_NODES];    // activation function
double b2[NUM_OF_HID2_NODES];                       // bias
double z2[NUM_OF_INPUT_DATA][NUM_OF_HID2_NODES];    // output vector

// Hidden_layer_1 to Hidden_layer_2 weight matrix
double w2[NUM_OF_HID2_NODES][NUM_OF_HID1_NODES];

// Hidden_layer_2 to output layer
double b3[NUM_OF_OUT_NODES];
double z3[NUM_OF_INPUT_DATA][NUM_OF_OUT_NODES];	// Predicted output vector

// Hidden layer to output layer weight matrix
double w3[NUM_OF_OUT_NODES][NUM_OF_HID2_NODES];

// Predicted values
double yhat[NUM_OF_INPUT_DATA][NUM_OF_OUT_NODES];
double yhat_eg[NUM_OF_OUT_NODES];

// Training data
double train_x[NUM_OF_INPUT_DATA][NUM_OF_FEATURES];				// Training data after normalization
double train_y[NUM_OF_INPUT_DATA][NUM_OF_OUT_NODES] = {{1}, {0}, {1}, {1}, {1}, {1},
                                                       {0}, {0}, {1}, {0}, {0}};  	// The expected (training) y values

int main() {
    // 0) Create input and output data set (example: input: {30, 40, 25} output: {1}).
    // Dataset size must be larger than 1.
    double raw_x[NUM_OF_INPUT_DATA][NUM_OF_FEATURES] = {
            {30, 40, 25}, {80, 55, 200}, {25, 30, 50}, {20, 35, 40}, {5, 45, 80}, {15, 40, 65},
            {-20, 30, 150}, {-30, 60, 130}, {22, 44, 66}, {8, 55, 21}, {50, 50, 50}};	// temp, humidity, air_q input values
    printf("Input data\n");
    matrix_print(NUM_OF_INPUT_DATA, NUM_OF_FEATURES, raw_x);
    // 1) Normalize input and output data.
    // Initialize weights and bias for all connections.
    // Weights should be initialized randomly and bias to zeros.
//    printf("------------------------------------ INITIALIZATION ------------------------------------\n");
    normalize_data_2d(NUM_OF_INPUT_DATA, NUM_OF_FEATURES, raw_x, train_x);	// Data normalization
    printf("Normalized data (train_x) \n");
    matrix_print(NUM_OF_INPUT_DATA, NUM_OF_FEATURES, train_x);

    // weights and bias initialization
    weights_random_initialization(NUM_OF_HID1_NODES, NUM_OF_FEATURES, w1);
    weights_random_initialization(NUM_OF_HID2_NODES, NUM_OF_HID1_NODES, w2);
    weights_random_initialization(NUM_OF_OUT_NODES, NUM_OF_HID2_NODES, w3);
//    printf("Initialized w1 \n");
//    matrix_print(NUM_OF_HID1_NODES, NUM_OF_FEATURES, w1);
//    printf("Initialized w2 \n");
//    matrix_print(NUM_OF_HID2_NODES, NUM_OF_HID1_NODES, w2);
//    printf("Initialized w3 \n");
//    matrix_print(NUM_OF_OUT_NODES, NUM_OF_HID2_NODES, w3);

    weightsB_zero_initialization(b1, NUM_OF_HID1_NODES);
    weightsB_zero_initialization(b2, NUM_OF_HID2_NODES);
    weightsB_zero_initialization(b3, NUM_OF_OUT_NODES);
//    printf("Initialized b1 \n");
//    matrix_print(1, NUM_OF_HID1_NODES, b1);
//    printf("Initialized b2 \n");
//    matrix_print(1, NUM_OF_HID2_NODES, b2);
//    printf("Initialized b3 \n");
//    matrix_print(1, NUM_OF_OUT_NODES, b3);
    int epoch_idx = 0;
    double cost = DBL_MAX;
    for (epoch_idx = 0 ; epoch_idx < NUM_OF_EPOCHS ; epoch_idx++) {
//        printf("------------------------------------ FORWARD PROPAGATION ------------------------------------\n");
        // 2) Implement forward propagation for all data-set examples.
        //    Store z1, z2, z3, a1, a2, yhat for later usage for backward propagation.

        for (int m = 0 ; m < NUM_OF_INPUT_DATA ; m++) {
            // Input -> 1st hidden layer
            linear_forward_nn(train_x[m], NUM_OF_FEATURES, z1[m], NUM_OF_HID1_NODES, w1, b1);
//            printf("Output vector (z1_A): %f\n", z1[m][0]);
//            printf("Output vector (z1_B): %f\n", z1[m][1]);
//            printf("Output vector (z1_C): %f\n", z1[m][2]);
//            printf("Output vector (z1_D): %f\n", z1[m][3]);
//            printf("Output vector (z1_E): %f\n", z1[m][4]);
            // activation in 1st hidden layer
            vector_relu(z1[m], a1[m], NUM_OF_HID1_NODES);
//            printf("a1 (z1 after relu) \n");
//            matrix_print(NUM_OF_INPUT_DATA, NUM_OF_HID1_NODES, a1);

            // 1st hidden -> 2nd hidden layer
            linear_forward_nn(a1[m], NUM_OF_HID1_NODES, z2[m], NUM_OF_HID2_NODES, w2, b2);
//            printf("Output vector (z2_F): %f\n", z2[m][0]);
//            printf("Output vector (z2_G): %f\n", z2[m][1]);
//            printf("Output vector (z2_H): %f\n", z2[m][2]);
//            printf("Output vector (z2_I): %f\n", z2[m][3]);

            // activation in 2nd hidden layer
            vector_relu(z2[m], a2[m], NUM_OF_HID2_NODES);
//            printf("a2 (z2 after relu) \n");
//            matrix_print(NUM_OF_INPUT_DATA, NUM_OF_HID2_NODES, a2);

            // 2nd hidden -> output layer
            linear_forward_nn(a2[m], NUM_OF_HID2_NODES, z3[m], NUM_OF_OUT_NODES, w3, b3);
//            printf("Output vector (z3): %f\r\n", z3[m][0]);

            // compute y^hat
            vector_sigmoid(z3[m], yhat[m], NUM_OF_OUT_NODES);
//            printf("yhat:  %f\n\r", yhat[m][0]);
        }

        // 3) Implement logistic regression cost function and calculate the cost for every iteration
        cost = compute_cost(NUM_OF_INPUT_DATA, yhat, train_y);
//        printf("\n----> COST: %f <----\n", cost);
        if (cost < ACCEPTED_COST_VALUE && epoch_idx > MIN_NUM_OF_EPOCHS) {
            break;
        }

//        printf("------------------------------------ BACKWARD PROPAGATION ------------------------------------\n");

        // 4) Implement backward propagation using data stored in z1, z2, z3, a1, a2, yhat
        //    finding this way dW1, dW2, dW3, db1, db2, db3.
        double dA1[NUM_OF_INPUT_DATA][NUM_OF_HID1_NODES];
        matrix_zero_values_initialization(NUM_OF_INPUT_DATA, NUM_OF_HID1_NODES, dA1);
        double dA2[NUM_OF_INPUT_DATA][NUM_OF_HID2_NODES];
        matrix_zero_values_initialization(NUM_OF_INPUT_DATA, NUM_OF_HID2_NODES, dA2);

        double dZ1[NUM_OF_INPUT_DATA][NUM_OF_HID1_NODES];
        matrix_zero_values_initialization(NUM_OF_INPUT_DATA, NUM_OF_HID1_NODES, dZ1);
        double dZ2[NUM_OF_INPUT_DATA][NUM_OF_HID2_NODES];
        matrix_zero_values_initialization(NUM_OF_INPUT_DATA, NUM_OF_HID2_NODES, dZ2);
        double dZ3[NUM_OF_INPUT_DATA][NUM_OF_OUT_NODES];
        matrix_zero_values_initialization(NUM_OF_INPUT_DATA, NUM_OF_OUT_NODES, dZ3);

        double dW1[NUM_OF_HID1_NODES][NUM_OF_FEATURES];
        matrix_zero_values_initialization(NUM_OF_HID1_NODES, NUM_OF_FEATURES, dW1);
        double dW2[NUM_OF_HID2_NODES][NUM_OF_HID1_NODES];
        matrix_zero_values_initialization(NUM_OF_HID2_NODES, NUM_OF_HID1_NODES, dW2);
        double dW3[NUM_OF_OUT_NODES][NUM_OF_HID2_NODES];
        matrix_zero_values_initialization(NUM_OF_OUT_NODES, NUM_OF_HID2_NODES, dW3);
        double db1[NUM_OF_HID1_NODES];
        weightsB_zero_initialization(db1, NUM_OF_HID1_NODES);
        double db2[NUM_OF_HID2_NODES];
        weightsB_zero_initialization(db2, NUM_OF_HID2_NODES);
        double db3[NUM_OF_OUT_NODES];
        weightsB_zero_initialization(db3, NUM_OF_OUT_NODES);

        // output layer -> 2nd hidden layer
        // dZ3 = A3 - Y = y^hat - y
        matrix_matrix_sub(NUM_OF_INPUT_DATA, NUM_OF_OUT_NODES, yhat, train_y, dZ3);
//        printf("dZ3 \n");
//        matrix_print(NUM_OF_INPUT_DATA, NUM_OF_OUT_NODES, dZ3);

        linear_backward(NUM_OF_OUT_NODES, NUM_OF_HID2_NODES, NUM_OF_INPUT_DATA, dZ3, a2, dW3, db3);
//        printf("dW3 \n");
//        matrix_print(NUM_OF_OUT_NODES, NUM_OF_HID2_NODES, dW3);
//        printf("db3 \n");
//        matrix_print(NUM_OF_OUT_NODES, 1, db3);

        double W3_T[NUM_OF_HID2_NODES][NUM_OF_OUT_NODES] = {{0},{0},{0},{0}};
        matrix_transpose(NUM_OF_OUT_NODES, NUM_OF_HID2_NODES, w3, W3_T);
//        printf("W3 \n");
//        matrix_print(NUM_OF_OUT_NODES, NUM_OF_HID2_NODES, w3);
//        printf("W3_T \n");
//        matrix_print(NUM_OF_HID2_NODES, NUM_OF_OUT_NODES, W3_T);

        matrix_matrix_multiplication(NUM_OF_HID2_NODES, NUM_OF_OUT_NODES, NUM_OF_INPUT_DATA, W3_T, dZ3, dA2);
//        printf("dA2 \n");
//        matrix_print(NUM_OF_INPUT_DATA, NUM_OF_HID2_NODES, dA2);

        relu_backward(NUM_OF_INPUT_DATA, NUM_OF_HID2_NODES, dA2, z2, dZ2);
//        printf("dZ2 \n");
//        matrix_print(NUM_OF_INPUT_DATA, NUM_OF_HID2_NODES, dZ2);

        // linear backward from 2nd hidden layer -> 1st hidden layer
        linear_backward(NUM_OF_HID2_NODES, NUM_OF_HID1_NODES, NUM_OF_INPUT_DATA, dZ2, a1, dW2, db2);
//        printf("dW2 \n");
//        matrix_print(NUM_OF_HID2_NODES, NUM_OF_HID1_NODES, dW2);
//        printf("db2 \n");
//        matrix_print(NUM_OF_HID2_NODES, 1, db2);

        double W2_T[NUM_OF_HID1_NODES][NUM_OF_HID2_NODES] = {{0, 0, 0, 0},{0, 0, 0, 0},{0, 0, 0, 0},{0, 0, 0, 0}, {0, 0, 0, 0}};
        matrix_transpose(NUM_OF_HID2_NODES, NUM_OF_HID1_NODES, w2, W2_T);
//        printf("W2 \n");
//        matrix_print(NUM_OF_HID2_NODES, NUM_OF_HID1_NODES, w2);
//        printf("W2_T \n");
//        matrix_print(NUM_OF_HID1_NODES, NUM_OF_HID2_NODES, W2_T);

        matrix_matrix_multiplication(NUM_OF_HID1_NODES, NUM_OF_HID2_NODES, NUM_OF_INPUT_DATA, W2_T, dZ2, dA1);
//        printf("dA1 \n");
//        matrix_print(NUM_OF_INPUT_DATA, NUM_OF_HID1_NODES, dA1);

        relu_backward(NUM_OF_INPUT_DATA, NUM_OF_HID1_NODES, dA1, z1, dZ1);
//        printf("dZ1 \n");
//        matrix_print(NUM_OF_INPUT_DATA, NUM_OF_HID1_NODES, dZ1);

        // 1st hidden -> Input layer
        linear_backward(NUM_OF_HID1_NODES, NUM_OF_FEATURES, NUM_OF_INPUT_DATA, dZ1, train_x, dW1, db1);
//        printf("dW1  \n");
//        matrix_print(NUM_OF_HID1_NODES, NUM_OF_FEATURES, dW1);
//        printf("db1  \n");
//        matrix_print(NUM_OF_HID1_NODES, 1, db1);

        // 5) Update parameters W1, W2, W3, b1, b2, b3
//        printf("------------------------------------ WEIGHTS UPDATE ------------------------------------\n");

        // update w1 and b1
        // W1 = W1 - learning_rate * dW1
        weights_update(NUM_OF_HID1_NODES, NUM_OF_FEATURES, learning_rate, dW1, w1);
//        printf("updated W1  \n");
//        matrix_print(NUM_OF_HID1_NODES, NUM_OF_FEATURES, w1);
        // b1 = b1 - learning_rate * db1
        weights_update(NUM_OF_HID1_NODES, 1, learning_rate, db1, b1);
//        printf("updated b1  \n");
//        matrix_print(NUM_OF_HID1_NODES, 1, b1);

        // update w2 and b2
        // W2 = W2 - learning_rate * dW2
        weights_update(NUM_OF_HID2_NODES, NUM_OF_HID1_NODES, learning_rate, dW2, w2);
//        printf("updated W2  \n");
//        matrix_print(NUM_OF_HID2_NODES, NUM_OF_HID1_NODES, w2);
        // b2 = b2 - learning_rate * db2
        weights_update(NUM_OF_HID2_NODES, 1, learning_rate, db2, b2);
//        printf("updated b2  \n");
//        matrix_print(NUM_OF_HID2_NODES, 1, b2);

        // update w3 and b3
        weights_update(NUM_OF_OUT_NODES, NUM_OF_HID2_NODES, learning_rate, dW3, w3);
//        printf("updated W3  \n");
//        matrix_print(NUM_OF_OUT_NODES, NUM_OF_HID2_NODES, w3);
        weights_update(NUM_OF_OUT_NODES, 1, learning_rate, db3, b3);
//        printf("updated b3  \n");
//        matrix_print(NUM_OF_OUT_NODES, 1, b3);
    }

    // 7) Create some example input and predict using taught neural network
    printf("\n------------------------------------ FINAL WEIGHTS AND BIAS ------------------------------------\n");
    printf("updated W1\n");
    matrix_print(NUM_OF_HID1_NODES, NUM_OF_FEATURES, w1);
    printf("updated b1\n");
    matrix_print(NUM_OF_HID1_NODES, 1, b1);

    printf("updated W2\n");
    matrix_print(NUM_OF_HID2_NODES, NUM_OF_HID1_NODES, w2);
    printf("updated b2\n");
    matrix_print(NUM_OF_HID2_NODES, 1, b2);

    printf("updated W3\n");
    matrix_print(NUM_OF_OUT_NODES, NUM_OF_HID2_NODES, w3);
    printf("updated b3\n");
    matrix_print(NUM_OF_OUT_NODES, 1, b3);

    printf("NN finished in epoch: %d. With cost: %f", epoch_idx, cost);

    printf("\n------------------------------------ PREDICT ------------------------------------\n");
//    double input_x_eg[1][NUM_OF_FEATURES] = {{25, 35, 25}};
    double input_x[1][NUM_OF_FEATURES] = {{0.5, 0.166667, 0.022346}}; // should be 1
//    normalize_data_2d(1,NUM_OF_FEATURES, input_x_eg, input_x);

	// compute z1
	linear_forward_nn(input_x[0], NUM_OF_FEATURES, z1[0], NUM_OF_HID1_NODES, w1, b1);
	// compute a1
	vector_relu(z1[0],a1[0], NUM_OF_HID1_NODES);
	// compute z2
	linear_forward_nn(a1[0], NUM_OF_HID1_NODES, z2[0], NUM_OF_HID2_NODES, w2, b2);
    // compute a2
    vector_relu(z2[0],a2[0], NUM_OF_HID2_NODES);
    // compute z3
    linear_forward_nn(a2[0], NUM_OF_HID2_NODES, z3[0], NUM_OF_OUT_NODES, w3, b3);
	// compute yhat
	vector_sigmoid(z3[0], yhat_eg, NUM_OF_OUT_NODES);
	printf("predicted:  %f. Expected: 1\n", yhat_eg[0]);

    return 0;
}
