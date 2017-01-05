#! /usr/bin/php
<?php
/* shpvla, convert ESRI Shapefile to Digistar VLA.
 * Copyright (C) 2004,2015-2017  John J. Foerch
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY JOHN J FOERCH ''AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL JOHN J FOERCH OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

define('LF', "\n");

$shpvla_basedir = dirname(__FILE__);
if (DIRECTORY_SEPARATOR != '/') {
    $shpvla_basedir = str_replace(DIRECTORY_SEPARATOR, '/', $shpvla_basedir);
}
if (substr($shpvla_basedir, -1) == '/') {
    $shpvla_basedir .= substr($shpvla_basedir, 0, strlen($shpvla_basedir) - 1);
}
define('SHPVLA_BASEDIR', $shpvla_basedir);

include_once(SHPVLA_BASEDIR.'/lib/geoformat_shapefile.php');


/*
 * Main
 */

function usage () {
    echo("shpvla.php <SHAPEFILE> ...".LF);
}

function main ($argc, &$argv) {
    if ($argc < 2) {
        usage();
    }
    for ($i = 1; $i < $argc; $i++) {
        //XXX: support 'draw' option (select shapes by regexp)
        $shape = new geoformat_shapefile(
            array('file' => $argv[$i]));
        $shape->make_vla();
    }
}
if (isset($argc)) {;
    main($argc, $argv);
}

?>