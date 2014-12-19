package main

import "fmt"

func main() {
	for i := 0; i < 8; i++ {
		for j := 0; j < 8; j++ {
			fmt.Printf("\033[3%d;4%dm 3%d \033[0m", i, j, i)
		}
		fmt.Print("\n")
	}
}
