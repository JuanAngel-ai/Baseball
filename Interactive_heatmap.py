from pybaseball import statcast
import matplotlib.pyplot as plt


def get_player_hits(player_name: str, start_dt: str, end_dt: str):
    data = statcast(start_dt=start_dt, end_dt=end_dt)
    hits = data[data["type"] == "X"].copy()
    hits = hits[["player_name", "hc_x", "hc_y", "events"]].dropna(subset=["hc_x", "hc_y"])
    hit_types = ["single", "double", "triple", "home_run"]
    hits = hits[hits["events"].isin(hit_types)]

    parts = player_name.strip().split()
    if len(parts) >= 2:
        first = parts[0]
        last = " ".join(parts[1:])
        alt_name = f"{last}, {first}"
        return hits[(hits["player_name"] == player_name) | (hits["player_name"] == alt_name)].copy()
    return hits[hits["player_name"] == player_name].copy()


def to_field_coords(df):
    out = df.copy()
    out["x"] = out["hc_x"] - 125
    out["y"] = 205 - out["hc_y"]  # flip so home plate is near y=0, outfield goes up
    return out


def plot_spray_heatmap(player_name=None, start_dt="2023-03-30", end_dt="2023-10-01"):
    if player_name:
        d = get_player_hits(player_name, start_dt, end_dt)
    else:
        data = statcast(start_dt=start_dt, end_dt=end_dt)
        d = data[data["type"] == "X"].copy()
        d = d[["player_name", "hc_x", "hc_y", "events"]].dropna(subset=["hc_x", "hc_y"])
        d = d[d["events"].isin(["single", "double", "triple", "home_run"])]

    if d.empty:
        label = player_name if player_name else "selection"
        raise ValueError(f"No hit data found for '{label}'.")

    d = to_field_coords(d)

    hit_styles = {
        "single":   {"color": "darkorange", "size": 8,  "alpha": 0.4, "label": "Single"},
        "double":   {"color": "dodgerblue", "size": 10, "alpha": 0.5, "label": "Double"},
        "triple":   {"color": "g",          "size": 12, "alpha": 0.6, "label": "Triple"},
        "home_run": {"color": "m",          "size": 15, "alpha": 0.5, "label": "Home Run"},
    }

    fig, ax = plt.subplots(figsize=(9.2, 6.1), dpi=110)
    fig.patch.set_facecolor("white")
    ax.set_facecolor("white")

    for event, style in hit_styles.items():
        z = d[d["events"] == event]
        if z.empty:
            continue
        ax.scatter(
            z["x"],
            z["y"],
            s=style["size"],
            c=style["color"],
            alpha=style["alpha"],
            marker=".",
            edgecolors="none",
            label=f"{style['label']} ({len(z)})",
        )

    title_name = player_name if player_name else "MLB"
    ax.set_title(f"{title_name} Spray Heatmap ({start_dt} to {end_dt})", fontsize=12)
    ax.set_xlim(-150, 150)
    ax.set_ylim(-20, 25 0)
    ax.set_aspect("equal", adjustable="box")
    ax.grid(False)
    ax.legend(loc="upper right", frameon=True, fontsize=8)

    plt.tight_layout()
    plt.show()


if __name__ == "__main__":
    plot_spray_heatmap(player_name=None)