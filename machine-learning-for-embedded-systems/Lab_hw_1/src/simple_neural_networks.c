#include "simple_neural_networks.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>

double single_in_single_out_nn(double  input, double weight) {
    return input * weight;
}

double weighted_sum(double * input, double * weight, uint32_t INPUT_LEN) {
    double output = 0;
    for (int i = 0 ; i < INPUT_LEN ; i++) {
        output += single_in_single_out_nn(input[i], weight[i]);
    }
    return output;
}

double multiple_inputs_single_output_nn(double * input, double *weight, uint32_t INPUT_LEN) {
    return weighted_sum(input, weight, INPUT_LEN);
}

void elementwise_multiple(double input_scalar, double *weight_vector, double *output_vector, double VECTOR_LEN) {
    for (int i = 0 ; i < VECTOR_LEN ; i++) {
        output_vector[i] += single_in_single_out_nn(input_scalar, weight_vector[i]);
    }
}

void single_input_multiple_output_nn(double input_scalar, double *weight_vector, double *output_vector, double VECTOR_LEN){
    elementwise_multiple(input_scalar, weight_vector,output_vector,VECTOR_LEN);
}

void matrix_vector_multiplication(double * input_vector, uint32_t INPUT_LEN, double * output_vector,
                                  uint32_t OUTPUT_LEN, double weights_matrix[OUTPUT_LEN][INPUT_LEN]) {
    for (int i = 0 ; i < INPUT_LEN ; i++) {
        for (int j = 0 ; j < OUTPUT_LEN ; j++) {
            output_vector[j] += weights_matrix[j][i] * input_vector[i];
        }
    }
}

void multiple_inputs_multiple_outputs_nn(double * input_vector, uint32_t INPUT_LEN, double * output_vector,
                                         uint32_t OUTPUT_LEN, double weights_matrix[OUTPUT_LEN][INPUT_LEN]) {
    matrix_vector_multiplication(input_vector,INPUT_LEN,output_vector,OUTPUT_LEN,weights_matrix);
}

void hidden_nn(double *input_vector, uint32_t INPUT_LEN,
               uint32_t HIDDEN_LEN, double in_to_hid_weights[HIDDEN_LEN][INPUT_LEN],
               uint32_t OUTPUT_LEN, double hid_to_out_weights[OUTPUT_LEN][HIDDEN_LEN], double *output_vector) {

    double hidden_layer_values[OUTPUT_LEN];
    matrix_vector_multiplication(input_vector, INPUT_LEN, hidden_layer_values, OUTPUT_LEN, in_to_hid_weights);
    matrix_vector_multiplication(hidden_layer_values, INPUT_LEN, output_vector, OUTPUT_LEN, hid_to_out_weights);
}

// Calculate the error using yhat (predicted value) and y (expected value)
double find_error(double yhat, double y) {
    return pow(yhat - y, 2);
}

void brute_force_learning(double input, double weight, double expected_value, double step_amount, uint32_t itr) {
    double up_prediction, down_prediction, up_error, down_error;
    for (int i = 0; i < itr; i++) {
        const double prediction = input * weight;
        const double error = find_error(prediction, expected_value);
        printf("Step: %d   Error: %f   Prediction: %f   Weight: %f\n", i, error, prediction, weight);

        if (error < 0.000001 && i > 1114) {
            break;
        }

        up_prediction = input * (weight + step_amount);
        up_error = find_error(up_prediction, expected_value);

        down_prediction = input * (weight - step_amount);
        down_error = find_error(down_prediction, expected_value);

        if (down_error < up_error) {
            weight -= step_amount;
        } else if (down_error > up_error) {
            weight += step_amount;
        }
    }
}

void linear_forward_nn(double *input_vector, uint32_t INPUT_LEN,
                       double *output_vector, uint32_t OUTPUT_LEN,
                       double weights_matrix[OUTPUT_LEN][INPUT_LEN], double *weights_b) {

    matrix_vector_multiplication(input_vector,INPUT_LEN, output_vector,OUTPUT_LEN,weights_matrix);

    for (int k = 0; k < OUTPUT_LEN; k++) {
        output_vector[k] += weights_b[k];
    }
}

double relu(double x) {
    return (0 >= x) ? 0 : x;
}

void vector_relu(double *input_vector, double *output_vector, uint32_t LEN) {
    for (int i = 0 ; i < LEN ; i++) {
        output_vector[i] = relu(input_vector[i]);
    }
}

double sigmoid(double x) {
    return 1/(1 + exp(-x));
}

void vector_sigmoid(double * input_vector, double * output_vector, uint32_t LEN) {
    for (int i = 0 ; i < LEN ; i++) {
        output_vector[i] = sigmoid(input_vector[i]);
    }
}

double compute_cost(uint32_t m, double yhat[m][1], double y[m][1]) {
    double cost = 0;
    for (int i = 0 ; i < m ; i++) {
        if (y[i][0] == 1) {
            if (yhat[i][0] <= 0) {
                continue;
            }
            cost += log(yhat[i][0]);
        } else if (y[i][0] == 0) {
            if ((1 - yhat[i][0]) <= 0) {
                continue;
            }
            cost += log(1 - yhat[i][0]);
        }
//        cost += y[i][0] * log(yhat[i][0]) + (1 - y[i][0]) * log(1 - yhat[i][0]);
    }
    return -cost/m;
}

void normalize_data_2d(uint32_t ROW, uint32_t COL, double input_matrix[ROW][COL], double output_matrix[ROW][COL]) {
    for (int i = 0 ; i < COL ; i++) {
        double col_max = -DBL_MAX;
        double col_min = DBL_MAX;
        // find max and min in the current column
        // viz https://stats.stackexchange.com/questions/70801/how-to-normalize-data-to-0-1-range
//        printf("values in COL: %d\n", i);
        for (int j = 0 ; j < ROW ; j++) {
//            printf("%f na pozici %d\t", input_matrix[j][i], j);
            if (input_matrix[j][i] > col_max) {
                col_max = input_matrix[j][i];
            }
            if (input_matrix[j][i] < col_min) {
                col_min = input_matrix[j][i];
            }
//            if (j == ROW - 1) {
//                printf("\n");
//            }
        }
//        printf("MIN: %f  MAX: %f\n", col_min, col_max);
        // divide each value in the column by col_max
        for (int j = 0 ; j < ROW ; j++) {
            if (col_min != col_max) {
                output_matrix[j][i] = (input_matrix[j][i] - col_min) / (col_max - col_min);
            } else {
                output_matrix[j][i] = 1;
            }
        }
    }
}

// Use this function to print matrix values for debugging
void matrix_print(uint32_t ROW, uint32_t COL, double A[ROW][COL]) {
    for (int i = 0 ; i < ROW ; i++) {
        for (int j = 0 ; j < COL ; j++) {
            printf(" %f ", A[i][j]);
        }
        printf("\n");
    }
    printf("\n\r");
}

void weights_random_initialization(uint32_t HIDDEN_LEN, uint32_t INPUT_LEN, double weight_matrix[HIDDEN_LEN][INPUT_LEN]) {
    double d_rand;

    /*Seed random number generator*/
    srand(1);

    for (int i = 0; i < HIDDEN_LEN; i++) {
        for (int j = 0; j < INPUT_LEN; j++) {
            /*Generate random numbers between 0 and 1*/
            d_rand = (rand() % 10);
            d_rand /= 10;
            weight_matrix[i][j] = d_rand;
        }
    }
}

void weightsB_zero_initialization(double *weightsB, uint32_t LEN) {
    memset(weightsB, 0, LEN * sizeof(weightsB[0]));
}

double relu_derivative(const double x) {
    return (x < 0) ? 0 : 1;
}

void relu_backward(uint32_t m, uint32_t LAYER_LEN, double dA[m][LAYER_LEN], double Z[m][LAYER_LEN],
                   double dZ[m][LAYER_LEN]) {
    for (int i = 0 ; i < m ; i++) {
        for (int j = 0 ; j < LAYER_LEN ; j++) {
            if (relu_derivative(Z[i][j]) == 1) {
                dZ[i][j] = dA[i][j];
            } else {
                dZ[i][j] = 0;
            }
        }
    }
}

void linear_backward(uint32_t LAYER_LEN, uint32_t PREV_LAYER_LEN, uint32_t m, double dZ[m][LAYER_LEN],
                     double A_prev[m][PREV_LAYER_LEN], double dW[LAYER_LEN][PREV_LAYER_LEN], double *db) {
    // update db
    for (int i = 0 ; i < LAYER_LEN ; i++) {
        double sum_dZ = 0;
        for (int j = 0 ; j < m ; j++) {
            sum_dZ += dZ[j][i];
        }
        db[i] = (1.0/m) * sum_dZ;
    }

    // transpose dZ
    double dZ_transposed[LAYER_LEN][m];
    matrix_transpose(m, LAYER_LEN, dZ, dZ_transposed);
    // compute dW
    matrix_matrix_multiplication(LAYER_LEN, m, PREV_LAYER_LEN, dZ_transposed, A_prev, dW);
    matrix_multiply_scalar(LAYER_LEN, PREV_LAYER_LEN, (1.0/m), dW, dW);
}

void matrix_matrix_sum(uint32_t MATRIX_ROW, uint32_t MATRIX_COL,
                       double input_matrix1[MATRIX_ROW][MATRIX_COL],
                       double input_matrix2[MATRIX_COL][MATRIX_COL],
                       double output_matrix[MATRIX_ROW][MATRIX_COL]) {
    for (int c = 0; c < MATRIX_ROW; c++) {
        for (int d = 0; d < MATRIX_COL; d++) {
            output_matrix[c][d] = input_matrix1[c][d] + input_matrix2[c][d];
        }
    }
}

void matrix_divide_scalar(uint32_t MATRIX_ROW, uint32_t MATRIX_COL, double scalar,
                          double input_matrix[MATRIX_ROW][MATRIX_COL],
                          double output_matrix[MATRIX_ROW][MATRIX_COL]) {
    for (int c = 0 ; c < MATRIX_ROW ; c++) {
        for (int d = 0 ; d < MATRIX_COL ; d++) {
            output_matrix[c][d] = input_matrix[c][d] / scalar;
        }
    }
}

void matrix_matrix_multiplication(uint32_t MATRIX1_ROW, uint32_t MATRIX1_COL, uint32_t MATRIX2_COL,
                                  double input_matrix1[MATRIX1_ROW][MATRIX1_COL],
                                  double input_matrix2[MATRIX1_COL][MATRIX2_COL],
                                  double output_matrix[MATRIX1_ROW][MATRIX2_COL]) {
    // initialize the output matrix with zeros
    for (int k = 0 ; k < MATRIX1_ROW ; k++) {
        memset(output_matrix[k], 0, MATRIX2_COL * sizeof(output_matrix[0][0]));
    }
    double sum = 0;
    for (int c = 0 ; c < MATRIX1_ROW ; c++) {
        for (int d = 0 ; d < MATRIX2_COL ; d++) {
            for (int k = 0 ; k < MATRIX1_COL ; k++) {
                sum += input_matrix1[c][k] * input_matrix2[k][d];
            }
            output_matrix[c][d] = sum;
            sum = 0;
        }
    }
}

// matrix subtraction
void matrix_matrix_sub(uint32_t MATRIX_ROW, uint32_t MATRIX_COL,
                       double input_matrix1[MATRIX_ROW][MATRIX_COL],
                       double input_matrix2[MATRIX_ROW][MATRIX_COL],
                       double output_matrix[MATRIX_ROW][MATRIX_COL]) {
    for (int c = 0 ; c < MATRIX_ROW ; c++) {
        for (int d = 0 ; d < MATRIX_COL ; d++) {
            output_matrix[c][d] = input_matrix1[c][d] - input_matrix2[c][d];
        }
    }
}

void weights_update(uint32_t MATRIX_ROW, uint32_t MATRIX_COL, double learning_rate,
                    double dW[MATRIX_ROW][MATRIX_COL],
                    double W[MATRIX_ROW][MATRIX_COL]) {
    for (int c = 0 ; c < MATRIX_ROW ; c++) {
        for (int d = 0 ; d < MATRIX_COL ; d++) {
            W[c][d] = W[c][d] - dW[c][d] * learning_rate;
        }
    }
}

void matrix_multiply_scalar(uint32_t MATRIX_ROW, uint32_t MATRIX_COL, double scalar,
                            double input_matrix[MATRIX_ROW][MATRIX_COL],
                            double output_matrix[MATRIX_ROW][MATRIX_COL]) {
    for (int c = 0 ; c < MATRIX_ROW ; c++) {
        for (int d = 0 ; d < MATRIX_COL ; d++) {
            output_matrix[c][d] = input_matrix[c][d] * scalar;
        }
    }
}

void matrix_transpose(uint32_t ROW, uint32_t COL, double A[ROW][COL], double A_T[COL][ROW]) {
    for (int i = 0 ; i < ROW ; i++) {
        for (int j = 0 ; j < COL ; j++) {
            A_T[j][i] = A[i][j];
        }
    }
}

void matrix_zero_values_initialization(uint32_t ROW, uint32_t COL, double matrix[ROW][COL]) {
    for (int i = 0 ; i < ROW ; i++) {
        for (int j = 0 ; j < COL ; j++) {
            matrix[i][j] = 0;
        }
    }
}
