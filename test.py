import subprocess
import sys
import time

def get_all_pitches():
	pitches = []
	for i in ('A', 'B', 'C', 'D', 'E', 'F', 'G'):
		for j in (1, 2, 3):
			pitches.append('{}{}'.format(i, j))
	return pitches

def get_available_chords(pitches):
	chords = []
	for i in range(len(pitches) - 2):
		for j in range(i + 1, len(pitches) - 1):
			for k in range(j + 1, len(pitches)):
				if i != j and j != k and i != k:
					chords.append((pitches[i], pitches[j], pitches[k]))
	return chords

def main():
	chords = get_available_chords(get_all_pitches())
	guesses = []
	times = []
	for chord in chords:
		prog = ['./Proj1Test'] + list(chord)
		tstart = time.time()
		stdout = str(subprocess.check_output(prog)).split(" ")
		telapsed = time.time() - tstart
		print("{} in {} guesses in {} s".format(" ".join(prog), stdout[-2], telapsed))
		sys.stdout.flush()
		guesses.append(int(stdout[-2]))
		times.append(telapsed)
	print("min number of guesses = {}".format(min(guesses)))
	print("avg number of guesses = {}".format(sum(sorted(guesses)) / len(guesses)))
	print("max number of guesses = {}".format(max(guesses)))
	print("min time used = {}".format(min(times)))
	print("avg time used = {}".format(sum(sorted(times)) / len(times)))
	print("max time used = {}".format(max(times)))

	sys.stdout.flush()

if __name__ == "__main__":
	main()
