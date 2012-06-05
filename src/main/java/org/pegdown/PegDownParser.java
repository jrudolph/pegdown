package org.pegdown;

import org.pegdown.ast.RootNode;

public interface PegDownParser {
    RootNode parse(char[] source);
}
