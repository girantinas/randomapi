# %%
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# %%
with open('./logs/bestProbSExprLog.txt') as f:
    d = {'ProbNumber': [], 'ProbOperator': [], 'ProbStructure': [], 'Value': []}
    for line in f:
        line_split = line.split()
        probs = tuple(map(lambda x: float(x), line_split[1][1:-1].split(',')))
        d['ProbNumber'].append(probs[0])
        d['ProbOperator'].append(probs[1])
        d['ProbStructure'].append(probs[2])
        d['Value'].append(float(line_split[-4]))

df = pd.DataFrame(data=d)


# %%
df_sorted = df.sort_values(by='Value')
argbest = df_sorted[-100:]
argworst = df_sorted[:100]
# %%
fig, axs = plt.subplots(2, 2)
fig.set_figheight(8)
fig.set_figwidth(8)
cols = df.columns[:-1]
for i, col in enumerate(cols):
    ax = axs[i // 2, i % 2]
    a, b, c = ax.hist(argbest[col], bins=10, align='left', range=(0.1, 1.1))
    mutation_type = col[4:]
    ax.set_xlabel(f"Probability of {mutation_type} Mutation")
    ax.set_ylabel("Count")
    ax.set_title(f"Best Stimuli ({mutation_type} Probs)")
fig.tight_layout()
plt.show()

# %%
fig, axs = plt.subplots(2, 2)
fig.set_figheight(8)
fig.set_figwidth(8)
colors = ['tab:red', 'cyan', 'yellow', 'lime', 'orange', 'tab:blue']
cols = df.columns[:-1]
for i, col in enumerate(cols):
    ax = axs[i // 2, i % 2]
    _, _, patches = ax.hist(argworst[col], bins=6, align='left', range=(0, 1.2))
    for c, p in zip(colors, patches):
        p.set_facecolor(c)
    mutation_type = col[4:]
    ax.set_xlabel(f"Probability of {mutation_type} Mutation")
    ax.set_ylabel("Count")
    ax.set_title(f"Worst Stimuli ({mutation_type} Probs{', no data for 1.0' if mutation_type == 'Immediate' else ''})")
fig.tight_layout()
plt.show()

# %%
df_timeseries = pd.read_csv("./logs/sexprNaiveMutateSeries.csv", header=None)
df0 = pd.read_csv("./logs/sexpr0PercentileMutateSeries.csv", header=None)
df25 = pd.read_csv("./logs/sexpr25PercentileMutateSeries.csv", header=None)
df50 = pd.read_csv("./logs/sexpr50PercentileMutateSeries.csv", header=None)
df75 = pd.read_csv("./logs/sexpr75PercentileMutateSeries.csv", header=None)
df100 = pd.read_csv("./logs/sexpr100PercentileMutateSeries.csv", header=None)

# %%
ax = plt.axes()

ax.plot(df_timeseries.T, label="Naive Mutation")
ax.plot(df50.T, label="25 Percentile Marked")
ax.plot(df25.T, label="50 Percentile Marked")
ax.plot(df75.T, label="75 Percentile Marked")
ax.plot(df100.T, label="100 Percentile Marked")
ax.set_yscale("log", base=10)
ax.set_xlabel("Number of Iterations")
ax.set_ylim(10 ** (-7), 5 * 10 ** (8))
ax.set_xlim()
ax.set_ylabel(r"Metric $\left[\frac{V}{(1.5)^{L}}\right]$")
ax.set_title("Feedback Metric Convergence for Different Mutation Strategies")
ax.legend()
plt.savefig('./figures/markedSExprConvergence.svg')
plt.show()
# %%

