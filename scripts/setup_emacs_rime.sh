#!/bin/bash

# =================================================================
# 脚本名称: setup_emacs_rime.sh
# 功能: 构建高度隔离且功能完整的 emacs-rime 配置环境 (绝对路径合并版)
# =================================================================

SOURCE_DIR="$HOME/Library/Rime"
TARGET_DIR="$HOME/.emacs.d/rime"
SYNC_DIR="$HOME/Library/Rime/sync"

echo "开始构建深度隔离的 emacs-rime 环境..."

# 1. 创建并进入目标目录
mkdir -p "$TARGET_DIR"
cd "$TARGET_DIR" || exit

# 2. 清理现有的所有符号链接
gfind . -type l -delete

# 3. 合并链接：YAML、Lua、Gram、Txt、Dict.yaml 等文件 (排除 installation.yaml)
echo "🔗 正在链接所有配置文件和词典..."
# 修正点：去掉了正则中小括号前的空格
gfind "$SOURCE_DIR" -maxdepth 1 -regextype posix-extended -regex ".*\.(yaml|lua|gram|txt|dict\.yaml)" | while read -r f; do
    filename=$(basename "$f")
    if [ "$filename" != "installation.yaml" ]; then
        ln -s "$f" "$filename"
    fi
done

# 4. 链接指定的子目录
DIRECTORIES=("cn_dicts" "en_dicts" "easy_en" "lua" "opencc" "others")
for dir in "${DIRECTORIES[@]}"; do
    if [ -d "$SOURCE_DIR/$dir" ]; then
        ln -s "$SOURCE_DIR/$dir" "$dir"
        echo "已链接目录: $dir"
    fi
done

# 5. 初始化独立的 installation.yaml
if [ ! -f "installation.yaml" ]; then
    cat <<EOF > installation.yaml
installation_id: "emacs-rime"
sync_dir: "$SYNC_DIR"
EOF
    echo "已生成独立的 installation.yaml"
fi

echo "-------------------------------------------------------"
echo "环境构建完成！"
echo "当前目录下的重要组件状态:"
ls -F | grep -E '\/$|\.lua$|\.gram$|installation.yaml'
echo "-------------------------------------------------------"
echo "提示: 请在 Emacs 中执行 M-x rime-deploy 使配置生效。"
