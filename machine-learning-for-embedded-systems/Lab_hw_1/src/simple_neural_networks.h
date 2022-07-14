#ifndef __SIMPLE_NEURAL_NETWORK
#define __SIMPLE_NEURAL_NETWORK

#include <stdint.h>

double single_in_single_out_nn(double input, double weight);
double multiple_inputs_single_output_nn(double * input, double *weight, uint32_t INPUT_LEN);
void single_input_multiple_output_nn( double input_scalar, double *weight_vector, double *output_vector, double VECTOR_LEN);
void multiple_inputs_multiple_outputs_nn(double * input_vector, uint32_t INPUT_LEN, double * output_vector,
		uint32_t OUTPUT_LEN, double weights_matrix[OUTPUT_LEN][INPUT_LEN]);
void hidden_nn( double *input_vector, uint32_t INPUT_LEN, uint32_t HIDDEN_LEN, double in_to_hid_weights[HIDDEN_LEN][INPUT_LEN],
		uint32_t OUTPUT_LEN, double hid_to_out_weights[OUTPUT_LEN][HIDDEN_LEN], double *output_vector);

double find_error( double yhat, double y);
void brute_force_learning( double input, double weight, double expected_value, double step_amount, uint32_t itr);

void weights_random_initialization(uint32_t HIDDEN_LEN, uint32_t INPUT_LEN, double weight_matrix[HIDDEN_LEN][INPUT_LEN]);
void linear_forward_nn(double * input_vector, uint32_t INPUT_LEN, double * output_vector,
		uint32_t OUTPUT_LEN, double weights_matrix[OUTPUT_LEN][INPUT_LEN], double * weights_b);
double relu(double x);
void vector_relu(double * input_vector, double * output_vector, uint32_t LEN);
void vector_sigmoid(double * input_vector, double * output_vector, uint32_t LEN);
double sigmoid(double x);
double compute_cost(uint32_t m, double yhat[m][1], double y[m][1]);
void normalize_data_2d(uint32_t ROW, uint32_t COL, double input_matrix[ROW][COL], double output_matrix[ROW][COL]);
void weightsB_zero_initialization(double * weightsB, uint32_t LEN);
void relu_backward(uint32_t m, uint32_t LAYER_LEN, double dA[m][LAYER_LEN], double Z[m][LAYER_LEN], double dZ[m][LAYER_LEN]);
void linear_backward(uint32_t LAYER_LEN, uint32_t PREV_LAYER_LEN, uint32_t m, double dZ[m][LAYER_LEN],
		double A_prev[m][PREV_LAYER_LEN], double dW[LAYER_LEN][PREV_LAYER_LEN], double * db);
void weights_update(uint32_t MATRIX_ROW, uint32_t MATRIX_COL, double learning_rate,
									double dW[MATRIX_ROW][MATRIX_COL],
									double W[MATRIX_ROW][MATRIX_COL]);
void matrix_matrix_multiplication(uint32_t MATRIX1_ROW, uint32_t MATRIX1_COL, uint32_t MATRIX2_COL,
									double input_matrix1[MATRIX1_ROW][MATRIX1_COL],
									double input_matrix2[MATRIX1_COL][MATRIX2_COL],
									double output_matrix[MATRIX1_ROW][MATRIX2_COL]);
void matrix_matrix_sum(uint32_t MATRIX_ROW, uint32_t MATRIX_COL,
									double input_matrix1[MATRIX_ROW][MATRIX_COL],
									double input_matrix2[MATRIX_COL][MATRIX_COL],
									double output_matrix[MATRIX_ROW][MATRIX_COL]);
void matrix_matrix_sub(uint32_t MATRIX_ROW, uint32_t MATRIX_COL,
									double input_matrix1[MATRIX_ROW][MATRIX_COL],
									double input_matrix2[MATRIX_ROW][MATRIX_COL],
									double output_matrix[MATRIX_ROW][MATRIX_COL]);
void matrix_transpose(uint32_t ROW, uint32_t COL, double A[ROW][COL], double A_T[COL][ROW]);
void matrix_multiply_scalar(uint32_t MATRIX_ROW, uint32_t MATRIX_COL, double scalar,
									double input_matrix[MATRIX_ROW][MATRIX_COL],
									double output_matrix[MATRIX_ROW][MATRIX_COL]);
void matrix_print(uint32_t ROW, uint32_t COL, double A[ROW][COL]);
void matrix_zero_values_initialization(uint32_t ROW, uint32_t COL, double input_matrix[ROW][COL]);

#endif
