

# 摘要

快速排序（QuickSort）是一种高效的分治算法，由 Tony Hoare 于 1960 年发明。本文将介绍快速排序的基本原理、在 C++ 中的实现、性能分析以及实际示例。


# 引言


## 快速排序概述

快速排序是一种原地排序算法，平均时间复杂度为 O(n log n)，最坏情况下为 O(n²)。它通过选择一个“枢轴”（pivot）将数组分为两部分：小于枢轴的元素和大于枢轴的元素，然后递归排序子数组。


## 适用场景

-   适合大数据集的内部排序。
-   与其他排序算法（如归并排序）比较，空间复杂度更低（O(log n) 递归深度）。


## 文章结构

-   算法原理
-   C++ 实现
-   示例与测试
-   复杂度分析
-   结论


# 算法原理


## 分治思想

快速排序的核心是“分治”：

1.  选择枢轴（pivot），通常取数组第一个、最后一个或随机元素。
2.  分区（Partition）：将数组重排，使小于 pivot 的元素在左，大于的在右。
3.  递归：对左右子数组重复步骤 1-2。


## 分区过程

假设数组 A[low..high]，pivot = A[high]：

-   i = low - 1
-   对于 j 从 low 到 high-1：
    -   如果 A[j] <= pivot，交换 A[++i] 和 A[j]
-   交换 A[i+1] 和 A[high]，返回 i+1 作为分区点。

伪代码：

    procedure quicksort(A, low, high)
        if low < high then
    	pivot_index := partition(A, low, high)
    	quicksort(A, low, pivot_index - 1)
    	quicksort(A, pivot_index + 1, high)


# C++ 实现


## 定义函数

partition 函数用于将给定数组分为两组，小于最后一个元素值的放到左边，大于最后一个元素值的放到右边。

    // 分区函数
    int partition(std::vector<int>& arr, int low, int high) {
        int pivot = arr[high];  // 选择最后一个元素作为 pivot
        int i = low - 1;        // 小于 pivot 的索引
    
        for (int j = low; j < high; ++j) {
    	if (arr[j] <= pivot) {
    	    ++i;
    	    std::swap(arr[i], arr[j]);
    	}
        }
        std::swap(arr[i + 1], arr[high]);
        return i + 1;
    }

quicksort 函数用于递归地对左右两边的数组进行排序。

    // 快速排序递归函数
    void quicksort(std::vector<int>& arr, int low, int high) {
        if (low < high) {
    	int pi = partition(arr, low, high);
    	quicksort(arr, low, pi - 1);
    	quicksort(arr, pi + 1, high);
        }
    }

quick\_sort 函数就是一个接口函数，简化形参。

    // 函数就是一个接口函数
    void quick_sort(std::vector<int>& arr) {
        quicksort(arr, 0, arr.size() - 1);
    }


## 完整代码

所需头文件

    #include <iostream>
    #include <vector>
    #include <algorithm>  // for std::swap
    
    using namespace std;

    #include <iostream>
    #include <vector>
    #include <algorithm>  // for std::swap
    
    using namespace std;
    // 分区函数
    int partition(std::vector<int>& arr, int low, int high) {
        int pivot = arr[high];  // 选择最后一个元素作为 pivot
        int i = low - 1;        // 小于 pivot 的索引
    
        for (int j = low; j < high; ++j) {
    	if (arr[j] <= pivot) {
    	    ++i;
    	    std::swap(arr[i], arr[j]);
    	}
        }
        std::swap(arr[i + 1], arr[high]);
        return i + 1;
    }
    // 快速排序递归函数
    void quicksort(std::vector<int>& arr, int low, int high) {
        if (low < high) {
    	int pi = partition(arr, low, high);
    	quicksort(arr, low, pi - 1);
    	quicksort(arr, pi + 1, high);
        }
    }
    // 函数就是一个接口函数
    void quick_sort(std::vector<int>& arr) {
        quicksort(arr, 0, arr.size() - 1);
    }
    int main() {
      std::vector<int> arr = {10, 7, 8, 9, 1, 5};
      std::cout << "原始数组: ";
      for (int num : arr) std::cout << num << " ";
      std::cout << std::endl;
    
      quick_sort(arr);
    
      std::cout << "排序后: ";
      for (int num : arr) std::cout << num << " ";
      std::cout << std::endl;
      return 0;
    }

    原始数组: 10 7 8 9 1 5 
    排序后: 1 5 7 8 9 10 


## 代码说明

-   实用 `std::vector` 作为动态数组。
-   `partition` 函数实现 Lomuto 分区方案（高效，原地）。
-   递归深度平均 log n，避免栈溢出（对于 n < 10^6 安全）。


# 示例与测试


## 简单示例

-   原始：10 7 8 9 1 5
-   排序：1 5 7 8 9 10


## 边界测试

-   空数组：无操作。
-   已排序数组：O(n^2)最坏，但随即 privot 可优化。
-   逆序数组：类似。

