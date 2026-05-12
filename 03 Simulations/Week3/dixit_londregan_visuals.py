"""Teaching visuals for Dixit and Londregan (1996).

The figures are conceptual illustrations of three mechanisms that are central to
the paper but easy to lose in the notation:

    1. party-specific delivery advantages;
    2. the marginal political return to transfers;
    3. the cancellation of group size in the marginal condition.

They use stylised values rather than empirical data.
"""

from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np


BLUE = "#4C78A8"
ORANGE = "#F58518"
GREEN = "#54A24B"
RED = "#E45756"
GREY = "#9D9D9D"
LIGHT_GREY = "#E6E6E6"


def figures_dir() -> Path:
    """Return the Week 3 simulation figures directory from repo root or Week3."""
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


def plot_leaky_bucket_delivery_advantage(out_dir: Path) -> None:
    """Visualise party-specific differences in the effectiveness of transfers."""
    groups = ["Group A\n(L advantage)", "Group B\n(R advantage)"]
    promised = np.array([100, 100])
    effective_l = np.array([80, 45])
    effective_r = np.array([45, 80])

    x = np.arange(len(groups))
    width = 0.27

    fig, ax = plt.subplots(figsize=(9, 5.2))
    ax.bar(
        x - width,
        promised,
        width,
        label="Promised transfer",
        color="white",
        edgecolor=GREY,
        linewidth=1.7,
        hatch="//",
    )
    bars_l = ax.bar(x, effective_l, width, label="Effective transfer from L", color=BLUE)
    bars_r = ax.bar(x + width, effective_r, width, label="Effective transfer from R", color=ORANGE)

    for bars, values in [(bars_l, effective_l), (bars_r, effective_r)]:
        for bar, value in zip(bars, values):
            leakage = int(100 - value)
            ax.text(
                bar.get_x() + bar.get_width() / 2,
                value + 3,
                rf"$\theta={leakage / 100:.2f}$",
                ha="center",
                fontsize=10,
            )

    ax.annotate(
        "Same promise,\ndifferent effective transfer",
        xy=(0, effective_l[0]),
        xytext=(0.45, 112),
        arrowprops={"arrowstyle": "->", "color": "#333333"},
        ha="center",
        va="bottom",
    )
    ax.set_title("Delivery advantages by party")
    ax.set_ylabel("Transfer value")
    ax.set_xticks(x)
    ax.set_xticklabels(groups)
    ax.set_ylim(0, 125)
    ax.legend(loc="upper right")
    fig.tight_layout()
    fig.savefig(out_dir / "dl86_leaky_bucket_delivery_advantage.png", bbox_inches="tight")
    plt.close(fig)


def plot_marginal_return_decomposition(out_dir: Path) -> None:
    """Show the three components of the marginal political return."""
    groups = ["Moderate\npoor", "Moderate\naffluent", "Partisan\npoor", "High\nleakage"]
    density_at_cutpoint = np.array([0.90, 0.90, 0.30, 0.75])
    marginal_utility = np.array([0.90, 0.35, 0.90, 0.80])
    delivery_efficiency = np.array([0.85, 0.85, 0.85, 0.35])
    marginal_return = density_at_cutpoint * marginal_utility * delivery_efficiency

    x = np.arange(len(groups))
    width = 0.19

    fig, ax = plt.subplots(figsize=(10, 5.4))
    ax.bar(x - 1.5 * width, density_at_cutpoint, width, label=r"Cutpoint density: $\phi_i(\Delta U_i)$", color=BLUE)
    ax.bar(x - 0.5 * width, marginal_utility, width, label=r"Marginal utility: $K_i C_i^{-\varepsilon}$", color=ORANGE)
    ax.bar(x + 0.5 * width, delivery_efficiency, width, label=r"Delivery: $1-\theta_{iL}$", color=GREEN)
    ax.bar(x + 1.5 * width, marginal_return, width, label="Marginal return", color=RED)

    for i, value in enumerate(marginal_return):
        ax.text(x[i] + 1.5 * width, value + 0.03, f"{value:.2f}", ha="center", fontsize=9)

    ax.set_title("Marginal political return to a transfer")
    ax.set_ylabel("Illustrative value")
    ax.set_xticks(x)
    ax.set_xticklabels(groups)
    ax.set_ylim(0, 1.12)
    ax.legend(loc="upper right", ncols=2)
    ax.text(
        0.02,
        0.96,
        r"$\phi_i(\Delta U_i)\times K_i C_i^{-\varepsilon}\times(1-\theta_{iL})$",
        transform=ax.transAxes,
        ha="left",
        va="top",
        bbox={"boxstyle": "round,pad=0.35", "facecolor": "white", "edgecolor": LIGHT_GREY},
    )
    fig.tight_layout()
    fig.savefig(out_dir / "dl86_marginal_return_decomposition.png", bbox_inches="tight")
    plt.close(fig)


def plot_group_size_cancellation(out_dir: Path) -> None:
    """Illustrate why group size cancels in the marginal return per dollar."""
    group_names = ["Small group", "Large group"]
    group_size = np.array([25, 100])
    budget = 100.0
    responsiveness = 0.5

    transfer_per_voter = budget / group_size
    cutpoint_shift = responsiveness * transfer_per_voter
    voters_moved = group_size * cutpoint_shift

    x = np.arange(len(group_names))
    width = 0.26

    fig, ax = plt.subplots(figsize=(9.5, 5.2))
    transfer_scaled = transfer_per_voter / transfer_per_voter.max()
    size_scaled = group_size / group_size.max()
    moved_scaled = voters_moved / voters_moved.max()

    ax.bar(x - width, size_scaled, width, color=BLUE, label="Number of voters")
    ax.bar(x, transfer_scaled, width, color=ORANGE, label="Transfer per voter")
    ax.bar(x + width, moved_scaled, width, color=GREEN, label="Voters moved")

    for i in range(len(group_names)):
        ax.text(x[i] - width, size_scaled[i] + 0.04, f"$N={group_size[i]}$", ha="center", fontsize=10)
        ax.text(x[i], transfer_scaled[i] + 0.04, f"${transfer_per_voter[i]:.0f}", ha="center", fontsize=10)
        ax.text(x[i] + width, moved_scaled[i] + 0.04, f"{voters_moved[i]:.0f}", ha="center", fontsize=10)

    ax.axhline(1, color="#333333", linestyle=":", linewidth=1)
    ax.set_title("Why group size cancels in the marginal calculation")
    ax.set_ylabel("Normalised value")
    ax.set_xticks(x)
    ax.set_xticklabels(group_names)
    ax.set_ylim(0, 1.25)
    ax.legend(loc="upper center", ncols=3)
    ax.text(
        0.5,
        -0.19,
        r"With a fixed budget: larger $N_i$ means more potential voters, but a smaller transfer to each voter.",
        transform=ax.transAxes,
        ha="center",
        va="top",
    )
    fig.tight_layout()
    fig.savefig(out_dir / "dl86_group_size_cancellation.png", bbox_inches="tight")
    plt.close(fig)


def main() -> None:
    apply_style()
    out_dirs = [figures_dir(), overleaf_figures_dir()]
    for out_dir in out_dirs:
        plot_leaky_bucket_delivery_advantage(out_dir)
        plot_marginal_return_decomposition(out_dir)
        plot_group_size_cancellation(out_dir)
    print("Saved Dixit-Londregan figures to:")
    for out_dir in out_dirs:
        print(f"  {out_dir}")


if __name__ == "__main__":
    main()
