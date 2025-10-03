---
title: C++使用智能指针示例
date: 2025-10-3 20:20:20
updated:
categories: [cpp]
tags: [智能指针,unique_ptr,shared_ptr, weak_ptr]
---
## 智能指针所需头文件

```cpp
#include <memory>  // 主要头文件，包含所有智能指针
```

<!-- more -->
## 完整示例

```cpp
#include <iostream>
#include <memory>      // 智能指针头文件
#include <vector>

class MyClass {
public:
    MyClass(int value) : data(value) {
        std::cout << "MyClass 构造函数: " << data << std::endl;
    }
    
    ~MyClass() {
        std::cout << "MyClass 析构函数: " << data << std::endl;
    }
    
    void print() const {
        std::cout << "数据: " << data << std::endl;
    }
    
private:
    int data;
};

int main() {
    std::cout << "=== unique_ptr 示例 ===" << std::endl;
    {
        // 1. unique_ptr - 独占所有权
        std::unique_ptr<MyClass> ptr1 = std::make_unique<MyClass>(100);
        ptr1->print();
        
        // 所有权转移
        std::unique_ptr<MyClass> ptr2 = std::move(ptr1);
        if (!ptr1) {
            std::cout << "ptr1 已为空" << std::endl;
        }
        ptr2->print();
    } // ptr2 自动释放


    std::cout << "\n=== shared_ptr 示例 ===" << std::endl;
    {
        // 2. shared_ptr - 共享所有权
        std::shared_ptr<MyClass> shared1 = std::make_shared<MyClass>(200);
        std::cout << "引用计数: " << shared1.use_count() << std::endl;
        
        {
            std::shared_ptr<MyClass> shared2 = shared1;
            std::cout << "引用计数: " << shared1.use_count() << std::endl;
            shared2->print();
        } // shared2 析构
        
        std::cout << "引用计数: " << shared1.use_count() << std::endl;
        shared1->print();
    } // shared1 自动释放
    
    std::cout << "\n=== weak_ptr 示例 ===" << std::endl;
    {
        // 3. weak_ptr - 观察而不拥有
        std::shared_ptr<MyClass> shared = std::make_shared<MyClass>(300);
        std::weak_ptr<MyClass> weak = shared;
        
        std::cout << "共享引用计数: " << shared.use_count() << std::endl;
        
        if (auto locked = weak.lock()) {
            std::cout << "weak_ptr 锁定成功" << std::endl;
            locked->print();
        } else {
            std::cout << "对象已被销毁" << std::endl;
        }
    } // shared 释放，weak 自动失效
    
    std::cout << "\n=== 数组管理示例 ===" << std::endl;
    {
        // 4. 管理动态数组
        std::unique_ptr<int[]> arr = std::make_unique<int[]>(5);
        for (int i = 0; i < 5; ++i) {
            arr[i] = i * 10;
            std::cout << "arr[" << i << "] = " << arr[i] << std::endl;
        }
    } // 数组自动释放
    
    std::cout << "\n=== 在容器中使用 ===" << std::endl;
    {
        // 5. 在标准容器中使用
        std::vector<std::shared_ptr<MyClass>> objects;
        objects.push_back(std::make_shared<MyClass>(400));
        objects.push_back(std::make_shared<MyClass // vector 中所有对象自动释放
    
    std::cout << "程序结束" << std::endl;
    return 0;
}
```

## 输出示例
```
=== unique_ptr 示例 ===
MyClass 构造函数: 100
数据: 100
ptr1 已为空
数据: 100
MyClass 析构函数: 100

=== shared_ptr 示例 ===
MyClass 构造函数: 200
引用计数: 1
引用计数: 2
数据: 200
引用计数: 1
数据: 200
MyClass 析构函数: 200

=== weak_ptr 示例 ===
MyClass 构造函数: 300
共享引用计数: 1
weak_ptr 锁定成功
数据: 300
MyClass 析构函数: 300

=== 数组管理示例 ===
arr[0] = 0
arr[1] = 10
arr[2] = 20
arr[3] = 30
arr[4] = 40

=== 在容器中使用 ===
MyClass 构造函数: 400
MyClass 构造函数: 500
数据: 400
数据: 500
MyClass 析构函数: 500
MyClass 析构函数: 400
程序结束
```

## 关键要点
- **`#include <memory>`** 包含所有智能指针类型
- **`std::make_unique`** (C++14) 和 **`std::make_shared`** 是推荐的创建方式
- 智能指针在作用域结束时自动释放内存，无需手动 `delete`

## C++14智能指针的创建方式
`std::unique_ptr` (C++11)
```cpp
// C++11 方式
std::unique_ptr<MyClass> ptr1(new MyClass(42));

// 或者使用 reset
std::unique_ptr<MyClass> ptr2;
ptr2.reset(new MyClass(42));
```

`std::shared_ptr` (C++11)
```cpp
// 直接构造
std::shared_ptr<MyClass> ptr1(new MyClass(42));

// 或者使用 reset
std::shared_ptr<MyClass> ptr2;
ptr2.reset(new MyClass(42));
```

问题与缺点
1. **可能的内存泄漏**：如果 new 成功但构造函数抛出异常
   ```cpp
   func(std::shared_ptr<MyClass>(new MyClass), other_func()); // 危险！
   ```

**C++14 引入的 `make_unique` 和 `make_shared` 解决了这些问题**，提供异常安全性和更好的性能。
