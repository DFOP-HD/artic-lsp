set -e

echo "Building Artic Language Server..."
echo "==================================================================================="

# Build lsp-framework
cd lsp-framework/ && cmake -DCMAKE_CXX_COMPILER="/usr/bin/clang++" -S . -B build && cmake --build build --parallel
cd ..

# Path to Thorin CMake: May need to change this on your machine
THORIN="$HOME/repos/anydsl/thorin";
THORIN_DIR="$THORIN/build/share/anydsl/cmake";

# Configure with LSP support
echo "Configuring CMake with LSP support..."
echo $THORIN_DIR
cmake -S . -B build \
  -DCMAKE_C_COMPILER=clang \
  -DCMAKE_CXX_COMPILER=clang++ \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
  -DARTIC_BUILD_LSP=ON \
  -DCMAKE_BUILD_TYPE=Debug \
  -DThorin_DIR=$THORIN_DIR

cp $THORIN/build/lib/libthorin.so build/lib/

# Build
echo "Building..."
cd build && make -j$(nproc)

echo ""
echo -e "\e[1;32mBuild complete!\e[0m"
echo "Binary location: $(pwd)/bin/artic"
echo ""

echo "Start the language server with: ./bin/artic --lsp"
echo "Or use the regular compiler:    ./bin/artic <file.art>"
