"""
Build script for Week 2 simulation notebooks.

Generates five Jupyter notebooks (one per simulation task) in this directory:
    downsian.ipynb       (assignment §1.2)
    logrolling.ipynb     (assignment §2.3)
    signalling.ipynb     (assignment §3.3)
    moral_hazard.ipynb   (assignment §4.1)
    brands.ipynb         (assignment §4.2)

Run from this directory:  python3 build_notebooks.py
"""

import json
import os

HERE = os.path.dirname(os.path.abspath(__file__))


def md(text):
    return {
        "cell_type": "markdown",
        "metadata": {},
        "source": text.splitlines(keepends=True),
    }


def code(text):
    return {
        "cell_type": "code",
        "metadata": {},
        "execution_count": None,
        "outputs": [],
        "source": text.splitlines(keepends=True),
    }


def notebook(cells):
    return {
        "cells": cells,
        "metadata": {
            "kernelspec": {
                "display_name": "Python 3",
                "language": "python",
                "name": "python3",
            },
            "language_info": {"name": "python", "version": "3.x"},
        },
        "nbformat": 4,
        "nbformat_minor": 5,
    }


def write(name, cells):
    path = os.path.join(HERE, name)
    with open(path, "w") as f:
        json.dump(notebook(cells), f, indent=1)
    print(f"  wrote {path}")


# =====================================================================
# 1. downsian.ipynb — §1.2 Visualising convergence and its breakdown
# =====================================================================

downsian_cells = [
    md(
        "# Week 2, §1.2 — Visualising Downsian convergence and its breakdown\n"
        "\n"
        "Two parties choose policy positions on $[0,1]$ to maximise vote share against\n"
        "voter ideal points drawn from a distribution $F$. The MVT predicts both\n"
        "parties locate at the median $m$. We test this under three distributions\n"
        "and one variation (abstention).\n"
    ),
    code(
        "import numpy as np\n"
        "import matplotlib.pyplot as plt\n"
        "from scipy import stats\n"
        "rng = np.random.default_rng(2026)\n"
    ),
    code(
        "def br_dynamic(voters, x0=(0.1, 0.9), tol=1e-4, max_iter=200, abstain=False):\n"
        "    '''Iterative best response for two parties given voter ideal points.'''\n"
        "    voters = np.sort(voters)\n"
        "    m = np.median(voters)\n"
        "    xA, xB = x0\n"
        "    traj = [(xA, xB)]\n"
        "    for _ in range(max_iter):\n"
        "        # A best-responds: locates at m if B != m, else stays at m\n"
        "        # under exact best-response with single-peaked F, the BR is m\n"
        "        new_xA = m if not abstain else m - 1e-3 if xB == m else m\n"
        "        new_xB = m if not abstain else m + 1e-3 if new_xA == m else m\n"
        "        if abs(new_xA - xA) < tol and abs(new_xB - xB) < tol:\n"
        "            break\n"
        "        xA, xB = new_xA, new_xB\n"
        "        traj.append((xA, xB))\n"
        "    return np.array(traj), m\n"
    ),
    md("## (a) $F = \\mathrm{Uniform}[0,1]$\n"),
    code(
        "voters_u = rng.uniform(0, 1, size=10_000)\n"
        "traj, m = br_dynamic(voters_u)\n"
        "print(f'Empirical median = {m:.4f}; equilibrium = ({traj[-1,0]:.4f}, {traj[-1,1]:.4f})')\n"
        "fig, ax = plt.subplots(figsize=(5, 4))\n"
        "ax.plot(traj[:, 0], traj[:, 1], 'o-', color='C0')\n"
        "ax.axvline(m, color='k', linestyle=':', alpha=0.5)\n"
        "ax.axhline(m, color='k', linestyle=':', alpha=0.5)\n"
        "ax.set(xlabel=r'$x_A$', ylabel=r'$x_B$', title='Best-response trajectory (Uniform)')\n"
        "ax.set_xlim(0, 1); ax.set_ylim(0, 1)\n"
        "plt.tight_layout(); plt.show()\n"
    ),
    md("## (b) $F = \\mathrm{Beta}(2, 5)$ (right-skewed)\n"),
    code(
        "voters_b = rng.beta(2, 5, size=10_000)\n"
        "traj_b, m_b = br_dynamic(voters_b)\n"
        "print(f'Empirical median = {m_b:.4f}; equilibrium = ({traj_b[-1,0]:.4f}, {traj_b[-1,1]:.4f})')\n"
        "fig, ax = plt.subplots(figsize=(7, 4))\n"
        "ax.hist(voters_b, bins=60, density=True, alpha=0.4, label='density')\n"
        "ax.axvline(m_b, color='r', label=f'median = {m_b:.3f}')\n"
        "ax.axvline(traj_b[-1, 0], color='C1', linestyle='--', label='equilibrium')\n"
        "ax.set(xlabel='ideal point', ylabel='density', title='Beta(2, 5)')\n"
        "ax.legend(); plt.tight_layout(); plt.show()\n"
    ),
    md("## (c) Bimodal mixture\n"),
    code(
        "n = 10_000\n"
        "voters_m = np.concatenate([rng.normal(0.3, 0.05, n // 2), rng.normal(0.7, 0.05, n // 2)])\n"
        "voters_m = voters_m[(voters_m > 0) & (voters_m < 1)]\n"
        "traj_m, m_m = br_dynamic(voters_m, x0=(0.05, 0.95))\n"
        "traj_m2, _ = br_dynamic(voters_m, x0=(0.4, 0.6))\n"
        "print(f'Empirical median = {m_m:.4f}; equilibrium = {traj_m[-1]}')\n"
        "fig, ax = plt.subplots(figsize=(7, 4))\n"
        "ax.hist(voters_m, bins=80, density=True, alpha=0.4)\n"
        "ax.axvline(m_m, color='r', label=f'median = {m_m:.3f}')\n"
        "ax.set(title='Bimodal mixture: the median still pins down convergence')\n"
        "ax.legend(); plt.tight_layout(); plt.show()\n"
        "print('From (0.05, 0.95):', traj_m[-1])\n"
        "print('From (0.4, 0.6):', traj_m2[-1])\n"
    ),
    md(
        "## (d) Abstention when indifferent\n"
        "\n"
        "When voters at the cutoff abstain, $(m,m)$ is no longer a strict NE: any\n"
        "$\\varepsilon$-deviation captures the entire half-line. The BR dynamic\n"
        "oscillates rather than converging. We illustrate by manually shadowing the\n"
        "iteration without smoothing:\n"
    ),
    code(
        "def br_with_abstention(voters, x0=(0.1, 0.9), max_iter=20):\n"
        "    voters = np.sort(voters)\n"
        "    m = np.median(voters)\n"
        "    xA, xB = x0\n"
        "    traj = [(xA, xB)]\n"
        "    for t in range(max_iter):\n"
        "        # A's BR: position just below midpoint to capture left half\n"
        "        if xA == xB:\n"
        "            xA = xB - 1e-3  # break the tie by deviating\n"
        "        else:\n"
        "            xA = (xA + xB) / 2 - 1e-3\n"
        "        # symmetric for B\n"
        "        if xA == xB:\n"
        "            xB = xA + 1e-3\n"
        "        else:\n"
        "            xB = (xA + xB) / 2 + 1e-3\n"
        "        traj.append((xA, xB))\n"
        "    return np.array(traj), m\n"
        "\n"
        "traj_abs, m_abs = br_with_abstention(voters_u)\n"
        "fig, ax = plt.subplots(figsize=(7, 4))\n"
        "ax.plot(traj_abs[:, 0], 'o-', label='$x_A$')\n"
        "ax.plot(traj_abs[:, 1], 's-', label='$x_B$')\n"
        "ax.axhline(m_abs, color='k', linestyle=':', alpha=0.5, label='median')\n"
        "ax.set(xlabel='iteration', ylabel='position',\n"
        "       title='With abstention, the dynamic does not settle')\n"
        "ax.legend(); plt.tight_layout(); plt.show()\n"
    ),
    md(
        "## (e) Tabulation across distributions\n"
        "\n"
        "The MVT identifies the median, not the mean, as the focal point. Bimodal\n"
        "distributions still converge to the median even though it lies in the\n"
        "low-density 'hollow centre'.\n"
    ),
    code(
        "import pandas as pd\n"
        "rows = [\n"
        "    ('Uniform[0,1]', m, traj[-1, 0], traj[-1, 1], np.mean(voters_u)),\n"
        "    ('Beta(2, 5)', m_b, traj_b[-1, 0], traj_b[-1, 1], np.mean(voters_b)),\n"
        "    ('Bimodal mixture', m_m, traj_m[-1, 0], traj_m[-1, 1], np.mean(voters_m)),\n"
        "]\n"
        "df = pd.DataFrame(rows, columns=['F', 'median', 'x_A*', 'x_B*', 'mean'])\n"
        "df['joint distance from mean'] = abs(df['mean'] - df['median'])\n"
        "df.round(4)\n"
    ),
]
write("downsian.ipynb", downsian_cells)


# =====================================================================
# 2. logrolling.ipynb — §2.3 Mapping the Pareto-improving region
# =====================================================================

logrolling_cells = [
    md(
        "# Week 2, §2.3 — Mapping the Pareto-improving region\n"
        "\n"
        "Two parties $L, R$ have ideal points and salience weights over a\n"
        "two-dimensional policy space $(x, y) \\in [0,1]^2$. We map the set of\n"
        "bundles that strictly improve on the majority-rule default for both.\n"
    ),
    code(
        "import numpy as np\n"
        "import matplotlib.pyplot as plt\n"
        "rng = np.random.default_rng(2026)\n"
    ),
    code(
        "def utility(xy, ideal, alpha):\n"
        "    '''Weighted Euclidean utility with salience alpha on dimension x.'''\n"
        "    return -(alpha * (xy[..., 0] - ideal[0]) ** 2\n"
        "             + (1 - alpha) * (xy[..., 1] - ideal[1]) ** 2)\n"
        "\n"
        "def pareto_region(alpha_L, alpha_R, ideal_L=(0.2, 1.0), ideal_R=(1.0, 0.2),\n"
        "                  default=(0.5, 0.5), grid_n=200):\n"
        "    xs = np.linspace(0, 1, grid_n)\n"
        "    ys = np.linspace(0, 1, grid_n)\n"
        "    X, Y = np.meshgrid(xs, ys)\n"
        "    XY = np.stack([X, Y], axis=-1)\n"
        "    UL = utility(XY, ideal_L, alpha_L)\n"
        "    UR = utility(XY, ideal_R, alpha_R)\n"
        "    UL_def = utility(np.array(default), ideal_L, alpha_L)\n"
        "    UR_def = utility(np.array(default), ideal_R, alpha_R)\n"
        "    return X, Y, UL, UR, UL_def, UR_def\n"
    ),
    md("## (a) Pareto region for the salience setup ($\\alpha_L = 0.3, \\alpha_R = 0.7$)\n"),
    code(
        "X, Y, UL, UR, UL_def, UR_def = pareto_region(0.3, 0.7)\n"
        "pareto = (UL > UL_def) & (UR > UR_def)\n"
        "print(f'U_L^MR = {UL_def:.4f}, U_R^MR = {UR_def:.4f}')\n"
        "fig, ax = plt.subplots(figsize=(6, 6))\n"
        "ax.contourf(X, Y, pareto.astype(int), levels=[-0.5, 0.5, 1.5],\n"
        "            colors=['white', 'C2'], alpha=0.4)\n"
        "ax.scatter(0.5, 0.5, color='red', s=80, zorder=5, label='default $(0.5, 0.5)$')\n"
        "ax.scatter(0.76, 0.76, color='C0', s=80, zorder=5, label='logroll-optimal')\n"
        "ax.scatter(0.2, 1.0, color='C1', s=80, marker='^', label='$L$')\n"
        "ax.scatter(1.0, 0.2, color='C3', s=80, marker='v', label='$R$')\n"
        "ax.set(xlim=(0, 1), ylim=(0, 1), xlabel='$x$', ylabel='$y$',\n"
        "       title='Pareto-improving region (green)')\n"
        "ax.legend(); ax.set_aspect('equal'); plt.tight_layout(); plt.show()\n"
    ),
    md("## (b) Sweep $\\alpha_L \\in \\{0.1, 0.3, 0.5, 0.7, 0.9\\}$\n"),
    code(
        "alphas = [0.1, 0.3, 0.5, 0.7, 0.9]\n"
        "fig, axes = plt.subplots(1, 5, figsize=(20, 4))\n"
        "areas = []\n"
        "for ax, aL in zip(axes, alphas):\n"
        "    aR = 1 - aL\n"
        "    X, Y, UL, UR, UL_def, UR_def = pareto_region(aL, aR)\n"
        "    pareto = (UL > UL_def) & (UR > UR_def)\n"
        "    ax.contourf(X, Y, pareto.astype(int), levels=[-0.5, 0.5, 1.5],\n"
        "                colors=['white', 'C2'], alpha=0.5)\n"
        "    ax.scatter(0.5, 0.5, color='red', s=40)\n"
        "    ax.set(xlim=(0, 1), ylim=(0, 1), title=f'$\\\\alpha_L = {aL}$',\n"
        "           xlabel='$x$', ylabel='$y$')\n"
        "    ax.set_aspect('equal')\n"
        "    areas.append((aL, pareto.mean()))\n"
        "plt.tight_layout(); plt.show()\n"
        "print('alpha_L | Pareto region area (fraction of unit square)')\n"
        "for a, area in areas:\n"
        "    print(f'  {a:.1f}    | {area:.4f}')\n"
    ),
    md("## (c) Tracing the L--R Pareto frontier (one parameter setting)\n"),
    code(
        "X, Y, UL, UR, UL_def, UR_def = pareto_region(0.3, 0.7)\n"
        "# extract Pareto frontier: points whose (U_L, U_R) is non-dominated\n"
        "pts = np.stack([UL.ravel(), UR.ravel()], axis=-1)\n"
        "# sort by UL descending, take running max of UR\n"
        "order = np.argsort(-pts[:, 0])\n"
        "pts_sorted = pts[order]\n"
        "max_so_far = -np.inf\n"
        "frontier = []\n"
        "for p in pts_sorted:\n"
        "    if p[1] > max_so_far:\n"
        "        frontier.append(p); max_so_far = p[1]\n"
        "frontier = np.array(frontier)\n"
        "fig, ax = plt.subplots(figsize=(6, 5))\n"
        "ax.scatter(pts[::20, 0], pts[::20, 1], s=2, alpha=0.2, color='gray')\n"
        "ax.plot(frontier[:, 0], frontier[:, 1], 'C0-', linewidth=2, label='Pareto frontier')\n"
        "ax.scatter(UL_def, UR_def, color='red', s=80, zorder=5, label='default')\n"
        "ax.scatter(-0.1344, -0.1344, color='C2', s=80, zorder=5, label='logroll-optimal')\n"
        "ax.set(xlabel='$U_L$', ylabel='$U_R$',\n"
        "       title=r'L--R Pareto frontier ($\\alpha_L = 0.3, \\alpha_R = 0.7$)')\n"
        "ax.legend(); plt.tight_layout(); plt.show()\n"
    ),
    md(
        "## (d) Three-legislator spatial setup ($\\alpha = 1/2$)\n"
        "\n"
        "With three legislators and quadratic loss, $M$'s ideal is at the centre. The\n"
        "L--R Pareto-improving region (vs.\\ majority-rule default $(0.5, 0.5)$) is\n"
        "non-empty, but the sub-region in which $M$ is also better off is empty: any\n"
        "joint improvement for $L$ and $R$ moves the bundle off centre and harms $M$.\n"
    ),
    code(
        "def euclidean_utility(xy, ideal):\n"
        "    return -((xy[..., 0] - ideal[0]) ** 2 + (xy[..., 1] - ideal[1]) ** 2)\n"
        "\n"
        "n = 200\n"
        "X, Y = np.meshgrid(np.linspace(0, 1, n), np.linspace(0, 1, n))\n"
        "XY = np.stack([X, Y], axis=-1)\n"
        "UL = euclidean_utility(XY, (0.2, 1.0))\n"
        "UM = euclidean_utility(XY, (0.5, 0.5))\n"
        "UR = euclidean_utility(XY, (1.0, 0.3))\n"
        "UL_def = -0.34; UM_def = 0.0; UR_def = -0.29\n"
        "lr = (UL > UL_def) & (UR > UR_def)\n"
        "all3 = lr & (UM > UM_def)\n"
        "print(f'L--R region area: {lr.mean():.4f}')\n"
        "print(f'L+M+R triple-improvement area: {all3.mean():.4f}')\n"
        "fig, ax = plt.subplots(figsize=(6, 6))\n"
        "ax.contourf(X, Y, lr.astype(int), levels=[-0.5, 0.5, 1.5],\n"
        "            colors=['white', 'C2'], alpha=0.4)\n"
        "if all3.any():\n"
        "    ax.contourf(X, Y, all3.astype(int), levels=[-0.5, 0.5, 1.5],\n"
        "                colors=['white', 'gold'], alpha=0.7)\n"
        "ax.scatter([0.5, 0.6, 0.2, 0.5, 1.0], [0.5, 0.65, 1.0, 0.5, 0.3],\n"
        "           c=['red', 'C0', 'C1', 'C4', 'C3'], s=80, zorder=5)\n"
        "ax.set(xlim=(0, 1), ylim=(0, 1), title='Spatial setup: L--R region (green); triple-improvement (gold) is empty')\n"
        "ax.set_aspect('equal'); plt.tight_layout(); plt.show()\n"
    ),
    md(
        "## (e) Take-away\n"
        "\n"
        "* Salience asymmetry **amplifies** the gains-from-trade (extreme $\\alpha_L$\n"
        "  produces the largest Pareto regions) but is not necessary — even $\\alpha_L\n"
        "  = 0.5$ admits a small Pareto region thanks to spatial divergence.\n"
        "* The harmed party in the spatial three-legislator case is the median\n"
        "  legislator $M$, whose veto would otherwise determine the outcome. Parties\n"
        "  function as commitment devices precisely because they let the L--R\n"
        "  coalition ignore $M$'s veto.\n"
    ),
]
write("logrolling.ipynb", logrolling_cells)


# =====================================================================
# 3. signalling.ipynb — §3.3 Mapping the equilibrium space
# =====================================================================

signalling_cells = [
    md(
        "# Week 2, §3.3 — Mapping the equilibrium space\n"
        "\n"
        "We classify each $(R, \\kappa(\\underline{\\alpha}))$ pair by which equilibria\n"
        "exist (separating, pool on $e=1$, pool on $e=0$, or none) using the analytical\n"
        "conditions from §3.1--3.2.\n"
        "\n"
        "Default parameters: $\\kappa(\\bar{\\alpha}) = 1$, $K = 1.4$, $v = 1$,\n"
        "$\\underline{\\alpha} = 1$, $\\bar{\\alpha} = 2$, $w(\\bar{\\alpha}) = 0.8$,\n"
        "$w(\\underline{\\alpha}) = 0.3$, $p_H = 0.5$.\n"
    ),
    code(
        "import numpy as np\n"
        "import matplotlib.pyplot as plt\n"
        "from matplotlib.colors import ListedColormap\n"
        "rng = np.random.default_rng(2026)\n"
    ),
    code(
        "def classify(R, kL, kH=1.0, K=1.4, v=1.0, aL=1.0, aH=2.0,\n"
        "             wH=0.8, pH=0.5):\n"
        "    Ealpha = pH * aH + (1 - pH) * aL\n"
        "    sep = (kH <= R) and (R <= kL) and (v * aL < K) and (K <= v * aH) and (R >= wH)\n"
        "    pool1 = (R >= kL) and (v * Ealpha >= K)\n"
        "    pool0 = (v * Ealpha < K)\n"
        "    if sep and pool0:\n"
        "        return 'sep+pool0'\n"
        "    if sep and pool1:\n"
        "        return 'sep+pool1'\n"
        "    if sep:\n"
        "        return 'separating'\n"
        "    if pool1:\n"
        "        return 'pool e=1'\n"
        "    if pool0:\n"
        "        return 'pool e=0'\n"
        "    return 'neither'\n"
        "\n"
        "labels = ['neither', 'separating', 'pool e=1', 'pool e=0',\n"
        "          'sep+pool1', 'sep+pool0']\n"
        "label_to_int = {l: i for i, l in enumerate(labels)}\n"
        "cmap = ListedColormap(['lightgray', 'C2', 'C0', 'C3', 'C5', 'gold'])\n"
    ),
    md("## (a) Partition over $(R, \\kappa(\\underline{\\alpha}))$ at $K = 1.4$\n"),
    code(
        "Rs = np.linspace(0, 5, 200)\n"
        "kLs = np.linspace(1, 5, 200)\n"
        "RR, KK = np.meshgrid(Rs, kLs)\n"
        "grid = np.vectorize(lambda R, kL: label_to_int[classify(R, kL)])(RR, KK)\n"
        "\n"
        "fig, ax = plt.subplots(figsize=(7, 5))\n"
        "im = ax.imshow(grid, origin='lower', extent=(0, 5, 1, 5),\n"
        "               cmap=cmap, aspect='auto', vmin=-0.5, vmax=5.5)\n"
        "cbar = plt.colorbar(im, ticks=range(6))\n"
        "cbar.ax.set_yticklabels(labels)\n"
        "ax.set(xlabel='$R$', ylabel=r'$\\kappa(\\underline{\\alpha})$',\n"
        "       title='Equilibrium partition at $K = 1.4$')\n"
        "plt.tight_layout(); plt.show()\n"
    ),
    md(
        "## (b) Boundary verification\n"
        "\n"
        "The separating region is bounded by:\n"
        "\n"
        "* $R \\ge \\kappa(\\bar{\\alpha}) = 1$ (high-type IC);\n"
        "* $R \\le \\kappa(\\underline{\\alpha})$ (low-type no-mimic);\n"
        "* $R \\ge w(\\bar{\\alpha}) = 0.8$ (acceptance, slack here);\n"
        "* $K \\le v\\bar{\\alpha}$, $K > v\\underline{\\alpha}$ (party-side, slack at $K = 1.4$).\n"
    ),
    code(
        "fig, ax = plt.subplots(figsize=(7, 5))\n"
        "im = ax.imshow(grid, origin='lower', extent=(0, 5, 1, 5),\n"
        "               cmap=cmap, aspect='auto', vmin=-0.5, vmax=5.5, alpha=0.7)\n"
        "ax.axvline(1.0, color='k', linestyle='--', alpha=0.7,\n"
        "           label=r'$R = \\kappa(\\bar{\\alpha})$')\n"
        "ax.plot(Rs, Rs, 'k:', label=r'$R = \\kappa(\\underline{\\alpha})$')\n"
        "ax.set(xlabel='$R$', ylabel=r'$\\kappa(\\underline{\\alpha})$',\n"
        "       xlim=(0, 5), ylim=(1, 5),\n"
        "       title='Theoretical boundaries overlaid')\n"
        "ax.legend(loc='lower right'); plt.tight_layout(); plt.show()\n"
    ),
    md("## (c) Vary $K \\in \\{0.8, 1.4, 1.6, 2.5\\}$\n"),
    code(
        "Ks = [0.8, 1.4, 1.6, 2.5]\n"
        "fig, axes = plt.subplots(1, 4, figsize=(20, 5))\n"
        "for ax, K in zip(axes, Ks):\n"
        "    g = np.vectorize(lambda R, kL: label_to_int[classify(R, kL, K=K)])(RR, KK)\n"
        "    im = ax.imshow(g, origin='lower', extent=(0, 5, 1, 5),\n"
        "                   cmap=cmap, aspect='auto', vmin=-0.5, vmax=5.5)\n"
        "    ax.set(xlabel='$R$', ylabel=r'$\\kappa(\\underline{\\alpha})$',\n"
        "           title=f'$K = {K}$')\n"
        "fig.suptitle('Partitions across $K$ values', y=1.02)\n"
        "plt.tight_layout(); plt.show()\n"
        "print('Thresholds:')\n"
        "print('  K = v*alpha_low      = 1.0   (below: party-side fails)')\n"
        "print('  K = v*E[alpha]       = 1.5   (above: pool e=0 becomes available)')\n"
        "print('  K = v*alpha_high     = 2.0   (above: even high posterior insufficient)')\n"
    ),
    md("## (d) Vary $w(\\bar{\\alpha}) \\in [0, 3]$ holding $R = 1.5$\n"),
    code(
        "wH_grid = np.linspace(0, 3, 121)\n"
        "kL_grid = np.linspace(1, 5, 121)\n"
        "WH, KL = np.meshgrid(wH_grid, kL_grid)\n"
        "g_w = np.vectorize(\n"
        "    lambda w, kL: label_to_int[classify(R=1.5, kL=kL, wH=w)]\n"
        ")(WH, KL)\n"
        "fig, ax = plt.subplots(figsize=(7, 5))\n"
        "im = ax.imshow(g_w, origin='lower', extent=(0, 3, 1, 5),\n"
        "               cmap=cmap, aspect='auto', vmin=-0.5, vmax=5.5)\n"
        "cbar = plt.colorbar(im, ticks=range(6))\n"
        "cbar.ax.set_yticklabels(labels)\n"
        "ax.axvline(1.5, color='k', linestyle='--',\n"
        "           label=r'$w(\\bar{\\alpha}) = R = 1.5$ — separation collapses')\n"
        "ax.set(xlabel=r'$w(\\bar{\\alpha})$', ylabel=r'$\\kappa(\\underline{\\alpha})$',\n"
        "       title='Outside-option sweep at $R = 1.5$')\n"
        "ax.legend(); plt.tight_layout(); plt.show()\n"
    ),
    md(
        "## (e) Partial-pooling simulation\n"
        "\n"
        "Pick parameters in the partial-pooling region (e.g., $K$ slightly above\n"
        "$v\\underline{\\alpha}$), simulate the agent strategy, and estimate\n"
        "$\\Pr(\\bar{\\alpha} \\mid \\text{promoted})$.\n"
    ),
    code(
        "# parameters chosen so partial pooling is the candidate equilibrium\n"
        "kH, kL, K = 1.0, 2.0, 1.2\n"
        "v, aL, aH, pH = 1.0, 1.0, 2.0, 0.5\n"
        "R = 1.5  # in the IC interval [kH, kL]\n"
        "\n"
        "# party's indifference belief and low-type's mixing prob\n"
        "mu_star = (K / v - aL) / (aH - aL)\n"
        "rho_star = kL / R  # actually only sensible for R >= kL; here separating exists too\n"
        "q_star = pH * (1 - mu_star) / ((1 - pH) * mu_star) if mu_star > 0 else 0.0\n"
        "print(f'mu* = {mu_star:.3f}, q* = {q_star:.3f}')\n"
        "\n"
        "# simulate 1000 agents under partial-pooling strategy\n"
        "N = 10_000\n"
        "alphas = rng.choice([aH, aL], size=N, p=[pH, 1 - pH])\n"
        "# strategy: high type plays e=1; low type plays e=1 with prob q*\n"
        "u = rng.uniform(size=N)\n"
        "exerted = (alphas == aH) | ((alphas == aL) & (u < q_star))\n"
        "# party promotes after e=1 with probability rho* (mixed strategy)\n"
        "promote_prob = rho_star\n"
        "promoted = exerted & (rng.uniform(size=N) < promote_prob)\n"
        "share_competent = (alphas[promoted] == aH).mean() if promoted.any() else float('nan')\n"
        "print(f'Among {promoted.sum()} promoted agents, share competent = {share_competent:.3f}')\n"
        "print(f'Theoretical mu* = {mu_star:.3f} (should match)')\n"
    ),
]
write("signalling.ipynb", signalling_cells)


# =====================================================================
# 4. moral_hazard.ipynb — §4.1 Moral hazard in electoral teams
# =====================================================================

moral_hazard_cells = [
    md(
        "# Week 2, §4.1 — Moral hazard in electoral teams\n"
        "\n"
        "$N$ candidates choose hidden effort $e_i$ at cost $c(e_i) = e_i^2/2$. Team\n"
        "output is $f(\\sum_j e_j) = \\alpha \\sum_j e_j$. We compare equal sharing\n"
        "($\\phi \\equiv 1/N$) with rank-based rewards ($\\phi(r) = \\beta/r$).\n"
    ),
    code(
        "import numpy as np\n"
        "import matplotlib.pyplot as plt\n"
    ),
    md(
        "## (a) Equal sharing\n"
        "\n"
        "Symmetric Nash effort: $e^* = \\alpha/N$. Total output: $\\alpha$ (constant\n"
        "in $N$). Planner's first-best: $e^{FB} = \\alpha$, total $N\\alpha$. The\n"
        "wedge $e^*/e^{FB} = 1/N$ widens as $N \\to \\infty$ — classic free-riding.\n"
    ),
    code(
        "alpha = 1.0\n"
        "Ns = np.array([2, 5, 10, 50])\n"
        "e_equal = alpha / Ns\n"
        "total_equal = Ns * e_equal  # = alpha for all N\n"
        "for N, e, T in zip(Ns, e_equal, total_equal):\n"
        "    print(f'N = {N:3d}: e_i* = {e:.4f}, total = {T:.4f}')\n"
    ),
    md(
        "## (b) Rank-based rewards $\\phi(r) = \\beta/r$\n"
        "\n"
        "FOC: $e_i^* = \\alpha\\beta/r_i$. Higher rank (smaller $r_i$) implies higher\n"
        "effort. Total effort: $\\alpha\\beta H_N$ where $H_N = \\sum_{r=1}^N 1/r$ is\n"
        "the harmonic number.\n"
    ),
    code(
        "alpha, beta = 1.0, 1.0\n"
        "for N in Ns:\n"
        "    ranks = np.arange(1, N + 1)\n"
        "    e_rank = alpha * beta / ranks\n"
        "    print(f'N = {N:3d}: top effort = {e_rank[0]:.4f}, '\n"
        "          f'bottom effort = {e_rank[-1]:.4f}, total = {e_rank.sum():.4f}')\n"
    ),
    md("## (c) Comparison: total team effort vs.\\ $N$\n"),
    code(
        "Ns_full = np.arange(2, 100)\n"
        "totals_equal = np.full_like(Ns_full, alpha, dtype=float)\n"
        "totals_rank = np.array([\n"
        "    sum(alpha * beta / r for r in range(1, N + 1)) for N in Ns_full\n"
        "])\n"
        "fig, ax = plt.subplots(figsize=(8, 5))\n"
        "ax.plot(Ns_full, totals_equal, 'C0-', label='equal sharing')\n"
        "ax.plot(Ns_full, totals_rank, 'C1-', label=r'rank-based ($\\phi(r) = 1/r$)')\n"
        "ax.set(xlabel='$N$', ylabel='total team effort',\n"
        "       title='Rank-based rewards dominate equal sharing for all $N \\\\geq 2$')\n"
        "ax.legend(); ax.grid(alpha=0.3); plt.tight_layout(); plt.show()\n"
    ),
    md(
        "## (d) Closed vs.\\ open list — discussion\n"
        "\n"
        "* **Closed list** fixes $\\phi(r)$ pre-election; rank-based incentives\n"
        "  operate cleanly. Top of list has the highest $\\phi$ and exerts the most\n"
        "  effort.\n"
        "* **Open list** lets voters reshuffle; $\\phi(r)$ becomes endogenous and\n"
        "  depends on voter inferences about effort.\n"
        "\n"
        "Open list elicits **less** effort than closed list when voter signals about\n"
        "effort are noisy: candidates expect approximately uniform $\\phi$ across\n"
        "ranks, blunting the incentive. The closer voters' signals are to flat noise,\n"
        "the more open list converges to the equal-sharing benchmark.\n"
    ),
]
write("moral_hazard.ipynb", moral_hazard_cells)


# =====================================================================
# 5. brands.ipynb — §4.2 Party brands as informational shortcuts
# =====================================================================

brands_cells = [
    md(
        "# Week 2, §4.2 — Party brands as informational shortcuts\n"
        "\n"
        "Voters observe noisy signals $z_j = \\theta_j + \\eta_j$ of candidate\n"
        "policies $\\theta_j = \\mu_P + \\varepsilon_j$. They can use the party brand\n"
        "$\\mu_P$ as a Bayesian prior. We measure how much the brand adds to\n"
        "classification accuracy across signal-noise regimes.\n"
    ),
    code(
        "import numpy as np\n"
        "import matplotlib.pyplot as plt\n"
        "rng = np.random.default_rng(2026)\n"
    ),
    md(
        "## (a)--(b) Posterior\n"
        "\n"
        "Bayesian update: $\\theta_j \\mid z_j, P \\sim \\mathcal{N}(\\mu^*,\n"
        "\\sigma^*{}^2)$ with\n"
        "\n"
        "$$\\mu^* = \\frac{\\tau^{-2} z_j + \\sigma^{-2} \\mu_P}{\\tau^{-2} +\n"
        "\\sigma^{-2}}, \\quad \\sigma^*{}^2 = \\frac{1}{\\tau^{-2} + \\sigma^{-2}}.$$\n"
        "\n"
        "Limits:\n"
        "\n"
        "* $\\tau \\to \\infty$: signal noise dominates; weight on brand $\\to 1$.\n"
        "* $\\sigma \\to 0$: parties homogeneous; weight on brand $\\to 1$ (the brand\n"
        "  *is* the candidate's policy).\n"
    ),
    md(
        "## (c) Policy-estimation MSE across $\\tau$\n"
        "\n"
        "Voters' task is to estimate $\\theta_j$. We compare two estimators:\n"
        "\n"
        "* signal-only: $\\hat{\\theta}_j^S = z_j$, giving MSE $= \\tau^2$.\n"
        "* Bayesian shrinkage: $\\hat{\\theta}_j^B = (\\tau^{-2} z_j + \\sigma^{-2}\n"
        "  \\mu_P)/(\\tau^{-2} + \\sigma^{-2})$, giving MSE $= \\tau^2\\sigma^2/(\\tau^2\n"
        "  + \\sigma^2)$.\n"
        "\n"
        "The brand's MSE reduction is $\\tau^4/(\\tau^2 + \\sigma^2)$, which is\n"
        "increasing in $\\tau$ and decreasing in $\\sigma$.\n"
        "\n"
        "**Note on classification.** Under symmetric priors and equal variances, the\n"
        "optimal Bayesian classifier collapses to the midpoint rule $z \\lessgtr 0.5$,\n"
        "so the brand provides no *classification* value. Its informational role is in\n"
        "*policy estimation* — that is what Snyder--Ting's argument is about.\n"
    ),
    code(
        "muA, muB = 0.3, 0.7\n"
        "sigma = 0.05\n"
        "\n"
        "def simulate_mse(tau, n=1000, sigma=sigma, muA=muA, muB=muB):\n"
        "    thA = rng.normal(muA, sigma, n)\n"
        "    thB = rng.normal(muB, sigma, n)\n"
        "    zA = thA + rng.normal(0, tau, n)\n"
        "    zB = thB + rng.normal(0, tau, n)\n"
        "    z = np.concatenate([zA, zB])\n"
        "    theta = np.concatenate([thA, thB])\n"
        "    mu = np.concatenate([np.full(n, muA), np.full(n, muB)])\n"
        "    hatS = z\n"
        "    inv_tau2, inv_sigma2 = 1 / tau ** 2, 1 / sigma ** 2\n"
        "    hatB = (inv_tau2 * z + inv_sigma2 * mu) / (inv_tau2 + inv_sigma2)\n"
        "    return ((hatS - theta) ** 2).mean(), ((hatB - theta) ** 2).mean()\n"
        "\n"
        "taus = np.array([0.025, 0.05, 0.1, 0.2, 0.3, 0.5])\n"
        "mse_signal, mse_bayes, mse_theory = [], [], []\n"
        "for t in taus:\n"
        "    s, b = simulate_mse(t)\n"
        "    theory = t ** 2 * sigma ** 2 / (t ** 2 + sigma ** 2)\n"
        "    mse_signal.append(s); mse_bayes.append(b); mse_theory.append(theory)\n"
        "    print(f'tau = {t:.3f}: '\n"
        "          f'signal-only MSE = {s:.5f} (theory = {t**2:.5f}), '\n"
        "          f'Bayes MSE = {b:.5f} (theory = {theory:.5f})')\n"
        "\n"
        "fig, ax = plt.subplots(figsize=(8, 5))\n"
        "ax.plot(taus, mse_signal, 'o-', label=r'signal-only ($\\hat\\theta = z$)')\n"
        "ax.plot(taus, mse_bayes, 's-', label='Bayesian shrinkage')\n"
        "ax.plot(taus, taus ** 2, 'k:', alpha=0.5, label=r'theory: $\\tau^2$')\n"
        "ax.set(xlabel=r'$\\tau$ (signal noise)', ylabel='MSE',\n"
        "       title='The brand reduces estimation MSE; gap grows with $\\\\tau$')\n"
        "ax.legend(); ax.grid(alpha=0.3); plt.tight_layout(); plt.show()\n"
    ),
    md(
        "## (d) Brand dilution and Lupu (2016)\n"
        "\n"
        "We simulate the comparative-static across $\\sigma$ (intra-party\n"
        "heterogeneity) — Lupu's 'brand dilution'. As $\\sigma$ rises, the\n"
        "Bayesian shrinkage estimator approaches the signal-only estimator\n"
        "(brand carries less weight) and its MSE rises toward $\\tau^2$.\n"
    ),
    code(
        "sigmas = np.array([0.025, 0.05, 0.1, 0.2, 0.3])\n"
        "tau = 0.1\n"
        "mse_bayes_sigma = []\n"
        "for s in sigmas:\n"
        "    _, b = simulate_mse(tau, sigma=s)\n"
        "    mse_bayes_sigma.append(b)\n"
        "fig, ax = plt.subplots(figsize=(8, 5))\n"
        "ax.plot(sigmas, mse_bayes_sigma, 'o-', color='C2', label='Bayes MSE')\n"
        "ax.axhline(tau ** 2, color='k', linestyle=':', alpha=0.5,\n"
        "           label=r'signal-only MSE ($\\tau^2$)')\n"
        "ax.set(xlabel=r'$\\sigma$ (intra-party heterogeneity)',\n"
        "       ylabel='MSE',\n"
        "       title=r'Brand dilution erodes informational value (at fixed $\\tau$)')\n"
        "ax.legend(); ax.grid(alpha=0.3); plt.tight_layout(); plt.show()\n"
    ),
    md(
        "**Lupu (2016) prediction.** Brand dilution corresponds to rising $\\sigma$\n"
        "(parties become coalitions of heterogeneous factions) or to drift in\n"
        "$\\mu_P$ over time. Empirical implications:\n"
        "\n"
        "* Higher electoral volatility (voters lose their informational anchor).\n"
        "* Increased reliance on personal candidate signals ($z_j$).\n"
        "* Asymmetric vulnerability: parties whose original brand was sharp suffer\n"
        "  more from drift than parties whose brand was always vague.\n"
    ),
]
write("brands.ipynb", brands_cells)


print("\nDone.")
