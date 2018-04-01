import subprocess
import os

with open(os.devnull, 'w') as devnull:
    for directory in ["more_paz_examples"]:
        filenames = os.listdir(f"./{directory}")
        input_filenames = [f for f in filenames
            if not f.split(".")[0].endswith("expected")]
        for f in input_filenames:
            result = subprocess.run(
                ["./paz", "-f", f"{directory}/{f}"],
                stdout=subprocess.PIPE,
                stderr=devnull)
            actual = result.stdout.decode("utf-8")
            filename = f"./{directory}/{f.split('.')[0]}_expected.paz"
            with open(filename) as handle:
                expected = handle.read()
            if expected == actual:
                print(f"Test {f:15s} passed")
            else:
                print(f"Test {f:15s} failed")
                print("Expected:")
                print("=========START=========")
                print(expected)
                print("==========EOF==========")
                print("Actual:")
                print("=========START=========")
                print(actual)
                print("==========EOF==========")
                print()
                print()