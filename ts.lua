#!/usr/local/bin/lua

function os.capture(cmd)
  local f = assert(io.popen(cmd, 'r'))
  local s = assert(f:read('*a'))
  f:close()
  s = string.gsub(s, '^%s+', '')
  s = string.gsub(s, '%s+$', '')
  s = string.gsub(s, '[\n\r]+', ' ')
  return s
end

function test(path, output)
  result = os.capture("./wielomianSzachowy ts/" .. path .. ".txt")
  if result == output then
    print(path .. " pass")
  else
    print(path .. " fail")
    print(path .. " : " .. result .. " /= " .. output)
  end
end

test("2b2-clean","[1,4,2]")
test("3b3-clean","[1,9,18,6]")
test("3b3-empty","[1,0,0,0]")
test("5b5-clean","[1,25,200,600,600,120]")
test("2b6-clean","[1,12,30]")
test("6b2-clean","[1,12,30]")
test("3b3-degen","[1,2,1,0]")
test("2b4-degen","[1,4,0]")
test("3b4-clean","[1,12,36,24]")
test("3b4-block","[1,8,16,7]")
