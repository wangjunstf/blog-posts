---
title: C++中 bitset 用法总结
date: 2025-09-30 20:53:22
updated:
categories: [cpp]
tags: [bitset]
---

`bitset` 是 C++ 标准库中的固定大小位集合容器，用于高效处理二进制位操作。
<!-- more -->
## 基本特性
- **固定大小**：在编译时确定大小
- **高效存储**：每个位只占 1 bit
- **支持位运算**：直接支持与、或、异或等位操作

## 常用操作

### 1. 构造和初始化
```cpp
#include <bitset>

bitset<8> b1;           // 全0：00000000
bitset<8> b2(0xAF);     // 十六进制：10101111
bitset<8> b3("1100");   // 字符串：00001100
```

### 2. 访问和修改
```cpp
bitset<8> bs("10101010");

bs[0] = 1;              // 设置第0位
bool bit = bs[2];       // 获取第2位
bs.set(3);              // 设置第3位为1
bs.reset(1);            // 设置第1位为0
bs.flip(4);             // 翻转第4位
```

### 3. 查询信息
```cpp
bs.size();              // 返回位数：8
bs.count();             // 返回1的个数
bs.any();               // 是否有1存在
bs.none();              // 是否全为0
bs.all();               // 是否全为1
```

### 4. 位运算
```cpp
bitset<8> a("11001100");
bitset<8> b("10101010");

auto c = a & b;         // 按位与
auto d = a | b;         // 按位或  
auto e = a ^ b;         // 按位异或
auto f = ~a;            // 按位取反
```

### 5. 转换
```cpp
bs.to_string();         // 转为字符串
bs.to_ulong();          // 转为unsigned long
bs.to_ullong();         // 转为unsigned long long
```

## 应用场景
- 状态标志存储
- 位掩码操作
- 集合运算（最多64个元素）
- 位级算法优化
