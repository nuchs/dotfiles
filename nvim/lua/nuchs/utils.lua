local utils = {}

function utils.load(module)
  local ok, err = pcall(require, module)

  if not ok then
    print("Failed to load " .. module)
    print(err)
    return
  end
end

return utils

