"""Teaching visuals for Lindbeck and Weibull (1987).

The figures are deliberately simple. They are meant to help students see the
main mechanism before working through the formal first-order condition:

    marginal electoral return = material responsiveness * electoral responsiveness

where material responsiveness is v'(c) and electoral responsiveness is f(0),
the density of voters close to partisan indifference.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import norm


@dataclass(frozen=True)
class Group:
    name: str
    income: float
    bias_mean: float
    size: int = 1


GROUPS = [
    # bias_mean is the expected value of b_i - a_i.
    # Negative values mean a bias toward A; positive values mean a bias toward B.
    Group("Poor\n(pro-A)", income=40.0, bias_mean=-1.1),
    Group("Middle\n(weak bias)", income=70.0, bias_mean=0.0),
    Group("Rich\n(pro-B)", income=120.0, bias_mean=1.1),
]

SIGMA = 1.0
COLORS = ["#4C78A8", "#F58518", "#54A24B"]


def figures_dir() -> Path:
    """Return the Week 3 figures directory from either repo root or this folder."""
    cwd = Path.cwd()
    if (cwd / "03 Simulations" / "Week3").exists():
        week3_dir = cwd / "03 Simulations" / "Week3"
    elif cwd.name == "Week3":
        week3_dir = cwd
    else:
        week3_dir = next(parent for parent in cwd.parents if parent.name == "Week3")

    out = week3_dir / "Figures"
    out.mkdir(parents=True, exist_ok=True)
    return out


def overleaf_figures_dir() -> Path:
    """Return the slide figures directory used by Overleaf."""
    repo_root = Path(__file__).resolve().parents[2]
    out = repo_root / "Figures" / "Week3"
    out.mkdir(parents=True, exist_ok=True)
    return out


def apply_style() -> None:
    plt.rcParams.update(
        {
            "figure.dpi": 120,
            "savefig.dpi": 300,
            "axes.spines.top": False,
            "axes.spines.right": False,
            "axes.grid": True,
            "grid.alpha": 0.25,
            "font.size": 11,
            "axes.titlesize": 15,
            "axes.labelsize": 12,
            "legend.frameon": False,
        }
    )


def utility_prime(consumption: np.ndarray | float) -> np.ndarray | float:
    """Log utility marginal utility: v'(c) = 1/c."""
    return 1 / consumption


def density_at_indifference(group: Group, sigma: float = SIGMA) -> float:
    """f_i(0), where b_i - a_i is normally distributed."""
    return norm.pdf(0, loc=group.bias_mean, scale=sigma)


def plot_party_bias_densities(out_dir: Path) -> None:
    """Visualise why f_i(0) is the density of electorally marginal voters."""
    t = np.linspace(-4, 4, 600)

    fig, ax = plt.subplots(figsize=(9, 5.2))
    for group, color in zip(GROUPS, COLORS):
        density = norm.pdf(t, loc=group.bias_mean, scale=SIGMA)
        f0 = density_at_indifference(group)
        ax.plot(t, density, color=color, linewidth=2.5, label=f"{group.name}: f(0)={f0:.2f}")
        ax.scatter([0], [f0], color=color, zorder=5)
        ax.vlines(0, 0, f0, color=color, linestyle=":", alpha=0.7)

    ax.axvline(0, color="black", linewidth=1.2)
    ax.text(0.05, ax.get_ylim()[1] * 0.93, "partisan\nindifference", ha="left", va="top")
    ax.set_title("Who is electorally movable? Density of partisan bias at zero")
    ax.set_xlabel(r"Non-material party bias: $b_i-a_i$")
    ax.set_ylabel("Density")
    ax.legend(loc="upper left")
    fig.tight_layout()
    fig.savefig(out_dir / "lw87_party_bias_densities.png", bbox_inches="tight")
    plt.close(fig)


def plot_marginal_return_components(out_dir: Path) -> None:
    """Decompose v'(income) * f(0) into material and electoral components."""
    names = [group.name for group in GROUPS]
    material = np.array([utility_prime(group.income) for group in GROUPS])
    electoral = np.array([density_at_indifference(group) for group in GROUPS])
    product = material * electoral

    # Normalise to make components visually comparable on one axis.
    material_norm = material / material.max()
    electoral_norm = electoral / electoral.max()
    product_norm = product / product.max()

    x = np.arange(len(GROUPS))
    width = 0.24

    fig, ax = plt.subplots(figsize=(9, 5.2))
    ax.bar(x - width, material_norm, width, label=r"Material: $v'(c)$", color="#4C78A8")
    ax.bar(x, electoral_norm, width, label=r"Electoral: $f(0)$", color="#F58518")
    ax.bar(x + width, product_norm, width, label=r"Return: $v'(c)f(0)$", color="#54A24B")

    for i, value in enumerate(product):
        ax.text(i + width, product_norm[i] + 0.03, f"{value:.4f}", ha="center", fontsize=9)

    ax.set_title("Marginal electoral return combines need and persuadability")
    ax.set_ylabel("Normalised value")
    ax.set_xticks(x)
    ax.set_xticklabels(names)
    ax.set_ylim(0, 1.22)
    ax.legend(loc="upper right")
    fig.tight_layout()
    fig.savefig(out_dir / "lw87_marginal_return_components.png", bbox_inches="tight")
    plt.close(fig)


def solve_equilibrium_consumption(groups: list[Group], sigma: float = SIGMA) -> tuple[np.ndarray, np.ndarray]:
    """Solve the three-group Director's Law example with log utility.

    The FOC is f_k(0) / c_k = lambda for each group k. Balanced budget is
    sum_k n_k(c_k - income_k) = 0.
    """
    incomes = np.array([group.income for group in groups])
    sizes = np.array([group.size for group in groups])
    f0 = np.array([density_at_indifference(group, sigma=sigma) for group in groups])

    lambda_star = np.sum(sizes * f0) / np.sum(sizes * incomes)
    consumption = f0 / lambda_star
    transfers = consumption - incomes
    return consumption, transfers


def plot_directors_law_simulation(out_dir: Path) -> None:
    """Show how the middle can receive the highest net income in equilibrium."""
    names = [group.name for group in GROUPS]
    incomes = np.array([group.income for group in GROUPS])
    consumption, transfers = solve_equilibrium_consumption(GROUPS)

    x = np.arange(len(GROUPS))
    width = 0.35

    fig, ax = plt.subplots(figsize=(9, 5.2))
    ax.bar(x - width / 2, incomes, width, label="Before redistribution", color="#9ecae9")
    ax.bar(x + width / 2, consumption, width, label="Equilibrium net income", color="#fdae6b")

    for i, transfer in enumerate(transfers):
        sign = "+" if transfer >= 0 else ""
        ax.text(
            x[i] + width / 2,
            consumption[i] + 4,
            f"{sign}{transfer:.0f}",
            ha="center",
            fontsize=10,
            fontweight="bold",
        )

    ax.axhline(np.average(incomes), color="black", linestyle=":", linewidth=1, label="Average resources")
    ax.set_title("Director's Law example: the least partisan group can gain most")
    ax.set_ylabel("Income / consumption")
    ax.set_xticks(x)
    ax.set_xticklabels(names)
    ax.legend(loc="upper left")
    fig.tight_layout()
    fig.savefig(out_dir / "lw87_directors_law_simulation.png", bbox_inches="tight")
    plt.close(fig)


def plot_stochastic_heterogeneity(out_dir: Path) -> None:
    """Show how uncertainty smooths the vote-probability response."""
    delta_v = np.linspace(-3, 3, 500)
    sigmas = [0.35, 0.8, 1.6]
    labels = ["Low uncertainty", "Medium uncertainty", "High uncertainty"]

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(11, 4.8))
    for sigma, label, color in zip(sigmas, labels, COLORS):
        p = norm.cdf(delta_v, loc=0, scale=sigma)
        density = norm.pdf(delta_v, loc=0, scale=sigma)
        ax1.plot(delta_v, p, color=color, linewidth=2.5, label=label)
        ax2.plot(delta_v, density, color=color, linewidth=2.5, label=label)

    ax1.axvline(0, color="black", linewidth=1)
    ax1.axhline(0.5, color="black", linewidth=1, linestyle=":")
    ax1.set_title("Vote probability")
    ax1.set_xlabel(r"Material utility advantage for A, $\Delta v_i$")
    ax1.set_ylabel(r"$p_i = F_i(\Delta v_i)$")
    ax1.legend(loc="lower right")

    ax2.axvline(0, color="black", linewidth=1)
    ax2.set_title("Marginal responsiveness")
    ax2.set_xlabel(r"Material utility advantage for A, $\Delta v_i$")
    ax2.set_ylabel(r"$f_i(\Delta v_i)$")
    ax2.legend(loc="upper right")

    fig.suptitle("Stochastic heterogeneity turns all-or-nothing voting into smooth responses", y=1.02)
    fig.tight_layout()
    fig.savefig(out_dir / "lw87_stochastic_heterogeneity.png", bbox_inches="tight")
    plt.close(fig)


def main() -> None:
    apply_style()
    out_dirs = [figures_dir(), overleaf_figures_dir()]
    for out_dir in out_dirs:
        plot_party_bias_densities(out_dir)
        plot_marginal_return_components(out_dir)
        plot_directors_law_simulation(out_dir)
        plot_stochastic_heterogeneity(out_dir)
    print("Saved Lindbeck-Weibull figures to:")
    for out_dir in out_dirs:
        print(f"  {out_dir}")


if __name__ == "__main__":
    main()
