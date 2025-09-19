add_library(libartic
    ../include/artic/ast.h
    ../include/artic/bind.h
    ../include/artic/cast.h
    ../include/artic/check.h
    ../include/artic/emit.h
    ../include/artic/lexer.h
    ../include/artic/loc.h
    ../include/artic/locator.h
    ../include/artic/log.h
    ../include/artic/parser.h
    ../include/artic/print.h
    ../include/artic/summoner.h
    ../include/artic/symbol.h
    ../include/artic/token.h
    ../include/artic/types.h
    arena.cpp
    ast.cpp
    bind.cpp
    check.cpp
    emit.cpp
    lexer.cpp
    log.cpp
    parser.cpp
    print.cpp
    summoner.cpp
    types.cpp)

set_target_properties(libartic PROPERTIES PREFIX "" CXX_STANDARD 20)

target_link_libraries(libartic PUBLIC ${Thorin_LIBRARIES})
target_include_directories(libartic PUBLIC ${Thorin_INCLUDE_DIRS} ../include)
target_compile_definitions(libartic PRIVATE -DARTIC_EXPORT)
if (${COLORIZE})
    target_compile_definitions(libartic PUBLIC -DCOLORIZE)
endif()

add_executable(artic main.cpp)
set_target_properties(artic PROPERTIES CXX_STANDARD 20)

target_compile_options(artic PRIVATE -Wno-deprecated-declarations)
target_compile_options(libartic PRIVATE -Wno-deprecated-declarations)

target_compile_definitions(artic PUBLIC -DARTIC_VERSION_MAJOR=${PROJECT_VERSION_MAJOR} -DARTIC_VERSION_MINOR=${PROJECT_VERSION_MINOR})
target_link_libraries(artic PUBLIC libartic)

# LSP support
option(ARTIC_BUILD_LSP "Build Artic with Language Server Protocol support" ON)

if(ARTIC_BUILD_LSP)
    # Add LSP source files
    target_sources(artic PRIVATE
        ls/server.cpp
        ls/workspace.cpp
        ls/crash.cpp
        ls/compile.cpp
        ls/config.cpp
    )
    
    # Include LSP framework headers
    target_include_directories(artic PRIVATE
        ../lsp-framework  # For lsp/ includes
        ../lsp-framework/build/generated  # For generated messages.h and types.h
    )
    
    # Link with LSP framework library
    target_link_libraries(artic PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/../lsp-framework/build/liblsp.a
    )
    
    # Enable LSP compilation
    target_compile_definitions(artic PRIVATE 
        ENABLE_LSP=1
    )
endif()

if (Thorin_HAS_JSON_SUPPORT)
    target_compile_definitions(artic PUBLIC -DENABLE_JSON)
    if (NOT TARGET nlohmann_json)
        find_package(nlohmann_json 3.2.0 REQUIRED)
    endif()
    target_link_libraries(artic PUBLIC nlohmann_json::nlohmann_json)
endif()
if (Thorin_HAS_LLVM_SUPPORT)
    target_compile_definitions(artic PUBLIC -DENABLE_LLVM)
    llvm_config(artic ${AnyDSL_LLVM_LINK_SHARED} core support)
endif ()
if (Thorin_HAS_SPIRV_SUPPORT)
    target_compile_definitions(artic PUBLIC -DENABLE_SPIRV)
endif ()
