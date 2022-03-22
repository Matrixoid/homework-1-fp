#include<stdio.h>
#include<stdlib.h>
#include <omp.h>

int main(int argc, char** argv) {

    const char* input_filename = argv[2];
    const char* output_filename = argv[3];
    float coef = atof(argv[4]);

    FILE* input_file = fopen(input_filename, "rb");
    if (!input_file) {
        printf("Unable to open file '%s'\n", input_filename);
        return -1;
    }

    char buff[16];
    fgets(buff, sizeof(buff), input_file);

    if (buff[0] != 'P' && buff[1] != '5' && buff[1] != '6') {
        printf("Invalid image format (must be 'P5' or 'P6')\n");
        return -1;
    }

    bool fn = buff[1] == '5';

    int width, height, rgb;
    fscanf(input_file, "%d %d", &width, &height);
    fscanf(input_file, "%d", &rgb);

    if (rgb != 255) {
        printf("'%s' does not have 8-bits components\n", input_filename);
        return -1;
    }

    unsigned char* data = (unsigned char*)malloc(width * height * 3);

    fread(data, 3 * width, height, input_file);

    fclose(input_file);

    int glowgist[256];

    for (int i = 0; i < 256; i++) {
        glowgist[i] = 0;
    }

    #pragma omp parallel
    {
        #pragma omp for
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width * 3; j += 3) {
                #pragma omp atomic
                glowgist[data[j + i * width * 3]]++;
                #pragma omp atomic
                glowgist[data[j + 1 + i * width * 3]]++;
                #pragma omp atomic
                glowgist[data[j + 2 + i * width * 3]]++;
            }
        }
    }

    int pixels_cnt = width * height * 3;
    float pixels = pixels_cnt - pixels_cnt * coef;
    bool flag = false;
    int begin = 0;

    #pragma omp parallel
    {
        #pragma omp for schedule(static, 1)
        for (int i = 0; i < 128; i++) {
            int sum = 0;
            for (int j = i; j < 256 - i; j++) {
                sum += glowgist[j];
            }
            if (sum < pixels && !flag) {
                flag = true;
                begin = i;
            }
        }
    }

    int b = 256 - begin * 2;
    int glowgist_t[256];
    for (int i = begin; i < 256 - begin; i++) {
        glowgist_t[i - begin] = glowgist[i];
    }

    int max = -1;
    int max_p = 0;
    int min = width * height * 3 + 1;
    int min_p = 0;

    #pragma omp parallel
    {
        int loc_max = -1;
        int p_max = 0;
        int loc_min = width * height * 3 + 1;
        int p_min = 0;
        #pragma omp for
        for (int i = 0; i < b; i++) {
            if (loc_max < glowgist_t[i]) {
                loc_max = glowgist_t[i];
                p_max = begin + i;
            }

            if (loc_min > glowgist_t[i]) {
                loc_min = glowgist_t[i];
                p_min = begin + i;
            }

        }

        #pragma omp critical
        {
            if (loc_max > max) {
                max = loc_max;
                max_p = p_max;
            }
            if (loc_min < min) {
                min = loc_min;
                min_p = p_min;
            }
        }
    }

    printf("%d %d", max_p, min_p);

    #pragma omp parallel
    {
        #pragma omp for
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width * 3; j += 3) {
                #pragma omp critical
                {
                    data[j + i * width * 3] = (data[j + i * width * 3] - min_p) * 255 / (max_p - min_p);
                }
            }
        }
    }
  
    FILE* output_file = fopen(output_filename, "wb");

    if (fn) {
        fprintf(output_file, "%s\n", "P5");
    }
    else {
        fprintf(output_file, "%s\n", "P6");
    }

    fprintf(output_file, "%d %d\n", width, height);
    fprintf(output_file, "%d\n", 255);
    fwrite(data, 3 * width, height, output_file);

    free(data);
    fclose(output_file);

}