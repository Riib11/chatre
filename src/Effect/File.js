import * as fs from 'fs'

export const _writeFile = content => filepath => {
  fs.writeFile(filepath, content, err => {
    if (err) console.error(err)
  })
}

