import fileinput
import itertools
import json

def grabFunctions(s):
    if "::" in s:
        (funcs, signature) = s.split("::")
        for func in funcs.split(","):
            yield f"""("{func.strip()}", "{signature.strip()}")"""

if __name__ == "__main__":
    print("module Questions exposing (questions)")
    print("")
    print("questions = [")
    print("\n,".join(itertools.chain.from_iterable(map(grabFunctions, fileinput.input()))))
    print("]")

