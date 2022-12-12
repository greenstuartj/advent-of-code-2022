package main

import (
	"fmt"
	"os"
	"strings"
)

type Coord struct {
	Row int
	Col int
}

func (c Coord) InBounds(height, width int) bool {
	return c.Row >=0 && c.Row < height && c.Col >=0 && c.Col < width
}

func (c Coord) Moves() []Coord {
	deltas := []int{ -1, 1 }
	var moves []Coord
	for _, d := range deltas {
		moves = append(moves, Coord{c.Row+d, c.Col})
	}
	for _, d := range deltas {
		moves = append(moves, Coord{c.Row, c.Col+d})
	}
	return moves
}

type Grid struct {
	grid [][]int
}

func (g Grid) GetCoord(c Coord) int {
	return g.grid[c.Row][c.Col]
}

func (g Grid) SetCoord(c Coord, i int) {
	g.grid[c.Row][c.Col] = i
}

func (g Grid) Dims() (int, int) {
	return len(g.grid), len(g.grid[0])
}

func MakeGraph(rows, cols int) Grid {
	var graph [][]int
	for i := 0; i < rows; i++ {
		var row []int
		for j := 0; j < cols; j++ {
			row = append(row, rows*cols)
		}
		graph = append(graph, row)
	}
	return Grid{graph}
}

func parseInput(input string) (Coord, Coord, Grid) {
	lines := strings.Split(strings.Trim(input, "\n"), "\n")
	var hill [][]int
	var start Coord
	var end Coord
	for i, line := range lines {
		var row []int
		for j, c := range line {
			if c == 'S' {
				c = 'a'
				start = Coord{i, j}
			} else if c == 'E' {
				c = 'z'
				end = Coord{i, j}
			}
			row = append(row, int(c)-97)
		}
		hill = append(hill, row)
	}
	return start, end, Grid{hill}
}

type Dijkstra struct {
	Graph Grid
}

func MakeDijkstra(hill Grid) Dijkstra {
	rows, columns := hill.Dims()
	graph := MakeGraph(rows, columns)
	return Dijkstra{ graph }
}

func (d Dijkstra) Start(start, end Coord, hill Grid, backwards bool) {
	d.Graph.SetCoord(start, 0)
	d.Path(start, end, hill, backwards)
}

func (d Dijkstra) Path(from, to Coord, hill Grid, backwards bool) {
	currentPath := d.Graph.GetCoord(from)
	currentHeight := hill.GetCoord(from)
	height, width := hill.Dims()
	moves := from.Moves()
	for _, move := range moves {
		if !move.InBounds(height, width) {
			continue
		}
		moveHeight := hill.GetCoord(move)
		if !backwards {
			if (moveHeight - currentHeight) > 1 {
				continue
			}
		} else {
			if (currentHeight - moveHeight) > 1 {
				continue
			}
		}
		movePath := d.Graph.GetCoord(move)
		if currentPath+1 >= movePath {
			continue
		}
		d.Graph.SetCoord(move, currentPath+1)
		d.Path(move, to, hill, backwards)
	}
}

func part1(start, end Coord, hill Grid) {
	dijkstra := MakeDijkstra(hill)
	dijkstra.Start(start, end, hill, false)
	fmt.Println(dijkstra.Graph.GetCoord(end))
}

func part2(start, end Coord, hill Grid) {
	dijkstra := MakeDijkstra(hill)
	dijkstra.Start(end, start, hill, true)
	min := 1000000
	for h, row := range hill.grid {
		for w, i := range row {
			if i != 0 {
				continue
			}
			candidate := dijkstra.Graph.GetCoord(Coord{h, w})
			if candidate < min {
				min = candidate
			}
		}
	}
	fmt.Println(min)
}

func main() {
	x, _ := os.ReadFile("data.txt")
	start, end, hill := parseInput(string(x))

	part1(start, end, hill)
	part2(start, end, hill)
}
