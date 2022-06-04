package main

import "fmt"

func main() {
	m := make(map[int]int)
	nums := []int{15, 5, 1, 4, 7, 0}
	for idx, v := range nums {
		m[v] = idx + 1
	}
	var last int = nums[len(nums)-1]
	for i := len(nums) + 1; i <= 30_000_000; i++ {
		v, ok := m[last]
		m[last] = i - 1
		if ok {
			last = i - 1 - v
		} else {
			last = 0
		}
	}
	fmt.Println("The solution:", last)
}
