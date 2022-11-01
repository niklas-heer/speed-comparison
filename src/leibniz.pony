use "files"
use "collections"
use "format"

actor Main
  new create(env:Env) =>
    let path = FilePath(
        FileAuth(env.root),"rounds.txt")
    
    match OpenFile(path)
    | let f :File =>
        let lines = f.lines()
        let line = try lines.next()? 
          else
            env.err.print("'rounds.txt' reading failed")
            return
          end
        let rounds = try line.u32()? + 2
          else
            env.err.print("invalid format of rounds")
            return
          end
        var pi = F64(1)
        var x = F64(1)
        for i in Range[U32](2,rounds) do
          x = x * -1.0
          let tmp = x / ( ( F64.from[U32](i) * 2.0 ) - 1.0 )
          pi = pi + tmp
        end
        pi = pi * 4.0
        env.out.print(Format.float(pi,FormatDefault,PrefixDefault,17))
    else
      env.err.print("'rounds.txt' open failed")
    end
