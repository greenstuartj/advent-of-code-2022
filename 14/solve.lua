
function file_to_lines(file)
   local lines = {}
   for line in io.lines(file) do
      table.insert(lines, line)
   end
   return lines
end

function signum(n)
   if n > 0 then
      return 1
   elseif n < 0 then
      return -1
   else
      return 0
   end
end

function wall(from, to)
   local coords = {}
   table.insert(coords, from)
   table.insert(coords, to)
   local fr = from.row
   local fc = from.col
   local tr = to.row
   local tc = to.col
   while not (fr == tr and fc == tc) do
      local row_d = tr - fr
      local col_d = tc - fc
      fr = math.floor(fr + signum(row_d))
      fc = math.floor(fc + signum(col_d))
      table.insert(coords, { row = fr, col = fc })
   end
   return coords
end

function lines_to_coords(lines)
   local walls = {}
   for _, line in pairs(lines) do
      local coords = {}
      for col, row in string.gmatch(line, "(%d+),(%d+)") do
         table.insert(coords, { row = tonumber(row), col = tonumber(col) })
      end
      for i=1, #coords-1 do
         for _, w in pairs(wall(coords[i], coords[i+1])) do
            table.insert(walls, w)
         end
      end
   end
   return walls
end

function make_grid(coords)
   local max_row = 0
   local min_col = 500
   local max_col = 0
   for _, c in pairs(coords) do
      if c.col < min_col then
         min_col = c.col
      end
      if c.col > max_col then
         max_col = c.col
      end
      if c.row > max_row then
         max_row = c.row
      end
   end
   local grid = {}
   for i=0, max_row do
      local row = {}
      for j=min_col-10000, max_col+10000 do
         row[j] = '.'
      end
      grid[i] = row
   end
   for _, coord in pairs(coords) do
      set_coord(coord, '#', grid)
   end
   return grid
end

function new_sand(grid)
   if grid[0][500] == 'o' then
      return false
   else
      grid[0][500] = 'o'
      return true
   end
end

function get_coord(coord, grid, expand)
   if not is_in_bounds(coord, grid) then
      if not expand then
         return false
      else
         grid[coord.row][coord.col] = '.'
         return '.'
      end
   end
   return grid[coord.row][coord.col]
end

function set_coord(coord, value, grid)
   grid[coord.row][coord.col] = value
end

function can_move_to(coord, grid, expand)
   local value = get_coord(coord, grid, expand)
   if not value then
      return "abyss"
   end
   return value and value == '.'
end

function is_in_bounds(coord, grid)
   if grid[coord.row] == nil then
      return false
   elseif grid[coord.row][coord.col] == nil then
      return false
   else
      return true
   end
end

function get_move(coord, grid, expand)
   local next_row = coord.row+1
   for _, delta in pairs({ 0, -1, 1 }) do
      local new_coord = { row = next_row, col = coord.col+delta }
      local cmt = can_move_to(new_coord, grid, expand)
      if cmt == "abyss" then
         return cmt
      end
      if can_move_to(new_coord, grid) then
         return new_coord
      end
   end
   return false
end

function move(from, to, grid)
   set_coord(from, '.', grid)
   set_coord(to, 'o', grid)
end

function sand_fall(from, grid, expand)
   set_coord(from, 'o', grid)
   local to = get_move(from, grid, expand)
   if not to or to == "abyss" then
      return to
   end
   move(from, to, grid)
   return to
end

function drop_sand(grid, expand, stop_function)
   local to = get_move({ row = 0, col = 500 }, grid, expand)
   while to do
      to = sand_fall(to, grid, expand)
      if stop_function(to, grid) then
         return false
      end
   end
   return true
end

function show_grid(grid)
   for i=0, #grid do
      local s = ""
      for _, c in pairs(grid[i]) do
         s = s .. c
      end
      print(s)
   end
end

function part1()
   local ls = lines_to_coords(file_to_lines("data.txt"))
   local g = make_grid(ls)
   local i = 0
   while drop_sand(g, false, function(to, grid) return to == "abyss" end) do
      i = i + 1
   end
   print(i)
end

function part2()
   local ls = lines_to_coords(file_to_lines("data.txt"))
   local g = make_grid(ls)
   g[#g+1] = {}
   for i, _ in pairs(g[#g-1]) do
      g[#g][i] = '.'
   end
   g[#g+1] = {}
   for i, _ in pairs(g[#g-1]) do
      g[#g][i] = '#'
   end
   local i = 0
   while drop_sand(g, true, function(to, grid) return not get_move({ row = 0, col = 500 }, grid) end) do
      i = i + 1
   end
   print(i+2)
end

function main()
   part1()
   part2()
end

main()
