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

define('TAU', 2 * M_PI);

/**
 * pythagorean distance formula in 3-space
 */
function distance3 ($x1, $y1, $z1, $x2, $y2, $z2) {
    $dx = $x1 - $x2;
    $dy = $y1 - $y2;
    $dz = $z1 - $z2;
    return sqrt($dx * $dx + $dy * $dy + $dz * $dz);
}

/**
 * ucfollow returns $str with the letter case altered to follow the casing
 * pattern of $pattern.  If pattern is shorter than str, characters of str at
 * indices higher than the highest index of pattern will retain their case.
 */
function ucfollow ($str, $pattern) {
    $ret = '';
    $stru = strtoupper($str);
    for ($i = 0; $i < strlen($pattern); ++$i) {
        $ret .= chr(ord($stru{$i}) | (ord($pattern{$i}) & 32));
    }
    $ret .= substr($str, strlen($pattern));
    return $ret;
}


/**
 * The following code is a workaround for php's unpack function
 * which does not have the capability of unpacking double precision
 * floats that were packed in a byte order other than the one of
 * the current machine.
 */
list ($shpvla_endiantest) = array_values(unpack('L1L', pack('V', 1)));
if ($shpvla_endiantest != 1) {
    define('BIG_ENDIAN_MACHINE', 1);
}

function unpack_workaround ($format, $data) {
    if (defined('BIG_ENDIAN_MACHINE')) {
        $ar = unpack($format, $data);
        $vals = array_values($ar);
        $f = explode('/', $format);
        $i = 0;
        foreach ($f as $f_k => $f_v) {
            $repeater = intval(substr($f_v, 1));
            if ($repeater == 0) {
                $repeater = 1;
            }
            if ($f_v{1} == '*') {
                $repeater = count($ar) - $i;
            }
            if ($f_v{0} != 'd') {
                $i += $repeater;
                continue;
            }
            $j = $i + $repeater;
            for ($a = $i; $a < $j; ++$a) {
                $p = pack('d', $vals[$i]);
                $p = strrev($p);
                list ($vals[$i]) = array_values(unpack('d1d', $p));
                ++$i;
            }
        }
        $a = 0;
        foreach ($ar as $ar_k => $ar_v) {
            $ar[$ar_k] = $vals[$a];
            ++$a;
        }
        return $ar;
    } else {
        return unpack($format, $data);
    }
}


class geoformat_shapefile {

    var $reportentries;

    var $base_name;
    var $dbf_name;
    var $shx_name;
    var $shp_name;

    var $dbf_content;
    var $shx_content;
    var $shp_content;

    var $shph;//file handle
    var $shp_content_seekpt = 0;

    var $dbf_keep_content = True;//always true for now. this will be used in
                                 //future to conserve memory.

    var $type;
    var $supported_types = array(1, 3, 5);

    var $style;
    var $linestyle;
    var $color;
    var $fillcolor;
    var $weight;
    var $dot;
    var $dotcolor = false;

    //match string or regular expression defining what shapes will be drawn.
    var $drawmatch = false;

    function geoformat_shapefile ($args) {
        if (! isset($args['file'])) {
            $this->ok=false;
            $this->error='No filename was provided.';
            return;
        }

        $this->base_name = $args['file'];
        $dotpos = strrpos($this->base_name, '.');
        $extension = substr($this->base_name, $dotpos);
        $this->base_name = substr($this->base_name, 0, $dotpos);
        $this->dbf_name = $this->base_name . ucfollow(".DBF", $extension);
        $this->shx_name = $this->base_name . ucfollow(".SHX", $extension);
        $this->shp_name = $this->base_name . ucfollow(".SHP", $extension);

        if (! (file_exists($this->dbf_name) |
               file_exists($this->shx_name) |
               file_exists($this->shp_name)))
        {
            $this->ok = false;
            $this->error = 'One or more of the DBF, the SHX, '.
                'or the SHP was not found.';
            return;
        }

        $shxh = fopen($this->shx_name, 'rb');
        $this->shx_content = fread($shxh, filesize($this->shx_name));
        fclose($shxh);

        $this->type = $this->shapetype();
        if (! in_array($this->type, $this->supported_types)) {
            $this->ok = false;
            $this->error = 'Shapefiles of type '.$this->type.
                ' are not yet supported.';
            return;
        }

        if (isset($args['dot'])) {
            $this->dot = $args['dot'];
        } else {
            $this->dot = 'dot';
        }
        if (isset($args['dotcolor'])) {
            $this->dotcolor = $args['dotcolor'];
        }
        if (isset($args['draw'])) {
            $this->drawmatch = $args['draw'];
        }
    }

    function make_vla () {
        $dbfdata=$this->DBF_Stage($this->drawmatch);
        $indexlist=$this->SHX_Stage($dbfdata);
        if (count($indexlist) == 0) {
            //XXX: stderr?
            //print('warning: Index stage returned nothing.');
        }
        $dbfdata = null;//free up this bit of memory
        $this->SHP_Stage($indexlist);
    }

    function DBF_Stage() {
        /* Optional argument 0 is a string or perl style regex to match
         * records in the dbf.  If it begins with / it will be treated as a
         * regex.  Otherwise it will be converted to upper case and matched
         * against the record, which will also be uppercased.
         */
        if (! $this->dbf_content) {
            $dbfh = fopen($this->dbf_name, 'rb');
            $this->dbf_content = fread($dbfh, filesize($this->dbf_name));
            fclose($dbfh);
        }

        $match='';
        if (func_num_args() > 0) {
            $match = func_get_arg(0);
            if (substr($match, 0, 1) == '/') {
                $use_regex = true;
            } else {
                $match = strtoupper($match);
                $use_regex = false;
            }
        }
        $dbfdata = array();

        $dbf_header = unpack(
            'Cversion/Cday/Cmonth/Cyear/Vnumrecords/vheaderlength/vrecordsize',
            substr($this->dbf_content, 0, 12));

        //NOTE
        //Erik Bachmann notes, concerning the record size element,
        //"+1 (deletion flag)"  Yet adding one throws the program off.

        for ($a = 0; $a < $dbf_header['numrecords']; $a++) {
            $record = substr($this->dbf_content,
                             $dbf_header['headerlength'] +
                             $dbf_header['recordsize'] * $a,
                             $dbf_header['recordsize']);
            if (ord(substr($record, 0, 1)) == 0x2a) {
                continue;
            }

            if ($match != '') {
                $flag = false;
                if ($use_regex) {
                    if (preg_match($match, $record) != 0) $flag=true;
                } else {
                    if (strpos(strtoupper($record), $match)) $flag=true;
                }
                if ($flag == false) {
                    continue;
                }
            }
            array_push($dbfdata, ($a));
        }

        if (! $this->dbf_keep_content) {
            $this->dbf_content = '';
        }

        return $dbfdata;
    }

    function SHX_Stage (&$dbfdata) {
        /* $dbfdata is an array of record indices to read.  If it is boolean
         * false, all records will be included.
         */
        $indexlist=array();
        if ($dbfdata === false) {
            $max = strlen($this->shx_content) - 8;
            for ($a = 100; $a <= $max; $a += 8) {
                list($var1, $var2) = array_values(
                    unpack("N2N", substr($this->shx_content, $a, 8)));
                $indexlist[$var1*2] = $var2*2;
            }
        } else {
            foreach ($dbfdata as $dbf_k => $dbf_v) {
                list($var1, $var2) = array_values(
                    unpack("N2N", substr($this->shx_content, 8*$dbf_v+100, 8)));
                // Convert Offset and Content length from words to bytes
                $indexlist[$var1*2] = $var2*2;
            }
        }
        return $indexlist;
    }

    function SHP_Stage (&$records) {
        $this->shph = fopen($this->shp_name, 'rb');
        switch ($this->type) {
            //case 1: $this->parse_point($records); break;
            case 3: $this->parse_polyline($records); break;
            case 5: $this->parse_polygon($records); break;
        }
        fclose($this->shph);
    }

    function get_shp_content ($offset, $length) {
        if (! defined('SHPVLA_SHAPEFILE_CONSERVE_MEMORY')) {
            if (! $this->shp_content) {
                $shph = fopen($this->shp_name, 'rb');
                $this->shp_content = fread($shph, filesize($this->shp_name));
                fclose($shph);
            }

            return substr($this->shp_content, $offset, $length);
        }

        $top = $this->shp_content_seekpt + strlen($this->shp_content);
        if (! ($offset >= $this->shp_content_seekpt &&
               $offset < $top &&
               $offset + $length <= $top))
        {
            if ($length > SHPVLA_SHAPEFILE_READ_SIZE) {
                $howmuch = $length;
            } else {
                $howmuch = SHPVLA_SHAPEFILE_READ_SIZE;
            }
            fseek($this->shph, $offset);
            $this->shp_content = fread($this->shph, $howmuch);
            $this->shp_content_seekpt = $offset;
        }
        return substr($this->shp_content,
                      $offset - $this->shp_content_seekpt,
                      $length);
    }

    /*
    function parse_point (&$projection, &$records) {
        $data = array();
        foreach ($records as $record_k => $record_v) {
            $ptrec = $this->get_shp_content($record_k + 8, 20);
            $p = 'Vtype/dX/dY';
            $a = unpack_workaround($p, $ptrec);
            if ($a['type'] == 1 &&
                $a['X'] >= $projection->xmin &&
                $a['X'] <= $projection->xmax &&
                $a['Y'] >= $projection->ymin &&
                $a['Y'] <= $projection->ymax)
            {
                $data[] = $a['X'];
                $data[] = $a['Y'];
            }
        }
        $geometry = array(
            'point' => array(
                'dot' => $this->dot,
                'dotcolor' => $this->dotcolor,
                'data' => $data
                )
            );
        $projection->draw($geometry);
    }
    */


    function parse_polyline (&$records) {
        foreach ($records as $record_k => $record_v) {
            $mlrec = $this->get_shp_content($record_k + 8, 44);
            $p = 'Vtype/d4bounds/Vnparts/Vnpoints';
            $a = unpack_workaround($p, $mlrec);
            $datasize = $a['nparts'] * 4 + $a['npoints'] * 16;
            $p = 'V'.$a['nparts'].'r/d'.($a['npoints']*2).'n';
            $mlrec = $this->get_shp_content($record_k + 52, $datasize);
            $b = unpack_workaround($p, $mlrec);
            $points_begin_idx = $record_k + 52 + $a['nparts'] * 4;

            // the shapefile spec says "Parts may or may not be connected to
            // one another."  I interpret this to mean they should not be
            // drawn connected.  If the author of the file intended the parts
            // to be connected, he could do so by making the end point of the
            // last part the same as the start point of the next one.
            // Otherwise, why make them separate parts?

            $curpart = 1;
            $command = 'P';
            for ($i = 1; $i <= $a['npoints']; ++$i) {
                //add this point to the end of $data
                $xidx = 'n'.(($i - 1) * 2 + 1);
                $yidx = 'n'.(($i - 1) * 2 + 2);
                //print($i .'   '.$b[$xidx] . '   '. $b[$yidx]."\n");
                $lon = TAU * $b[$xidx] / 360.0;
                $lat = TAU * $b[$yidx] / 360.0;
                $x = cos($lon) * cos($lat);
                $y = sin($lon) * cos($lat);
                $z = sin($lat);
                echo($command." ".$x." ".$z." ".$y." 1.0\n");
                $command = 'L';

                //new point if this is the last point in the current part
                $next_point_idx = $i * 16 + $points_begin_idx;
                if ($curpart < $a['nparts'] &&
                    $next_point_idx == $a['part'.($curpart+1)])
                {
                    $command = 'P';
                    ++$curpart;
                }
            }
        }
    }


    function parse_polygon (&$records) {
        foreach ($records as $record_k => $record_v) {
            # record_k=Offset record_v=Length
            $ar = unpack('Nctlen', $this->get_shp_content($record_k+4, 4));
            $ar['ctlen']*=2; # Words * 2 = Bytes
            $shaperec = $this->get_shp_content($record_k + 8, $ar['ctlen']);

            $rings=$this->ParsePolygonRecord($shaperec);

            // Rings will be written out as VLA sequences
            foreach ($rings as $points) {
                // iterate points by twos
                $odd = false;
                $lon = false;
                $lat = false;
                $x = false;
                $y = false;
                $z = false;
                $px = false;
                $py = false;
                $pz = false;
                $command = 'P';
                foreach ($points as $coord) {
                    if ($odd) {
                        // we have a point
                        $px = $x;
                        $py = $y;
                        $pz = $z;
                        $lat = TAU * $coord / 360.0;
                        $x = cos($lon) * cos($lat);
                        $y = sin($lon) * cos($lat);
                        $z = sin($lat);
                        // if ($px !== false && distance3($px, $py, $pz, $x, $y, $z) > 0.069)
                        //     echo("!!!!!!!!!!!!!!!\n");
                        echo($command." ".$x." ".$z." ".$y." 1.0\n");
                        $command = 'L';
                    } else {
                        $lon = TAU * $coord / 360.0;
                    }
                    $odd = ! $odd;
                }
            }
        }
    }

    function ParsePolygonRecord (&$record) {
        if (strlen($record) == 0) {
            return;
        }
        $upstr = 'VShapetype/d4d/V2V';
        list($ShapeType, $XMin, $YMin, $XMax, $YMax, $NumParts, $NumPoints) =
            array_values(unpack_workaround($upstr, substr($record, 0, 44)));
        if ($ShapeType == 0) {
            return;
        }
        $os=44 + $NumParts * 4;
        if ($NumPoints == 0) {
            //XXX: stderr?
            //print('error: no points in polygon');
        }
        $rings=array();//structure that will be returned
        $points=array();
        $startx = $starty = false;
        for ($a=0; $a<$NumPoints; ++$a) {
            list($x, $y)=
                array_values(unpack_workaround("d2d", substr($record, $os, 16)));
            $points[]=$x;
            $points[]=$y;
            if ($startx === false) {
                $startx = $x;
                $starty = $y;
            } elseif ($x === $startx && $y === $starty) {
                $startx = $starty = false;
                if ($points[1] !== end($points) &&
                    $points[0] !== prev($points))
                {
                    $points[] = $points[0];
                    $points[] = $points[1];
                }
                $rings[] = $points;
                $points = array();
            }
            $os += 16;
        }
        if ($points) {
            if ($points[1] !== end($points) &&
                $points[0] !== prev($points))
            {
                $points[] = $points[0];
                $points[] = $points[1];
            }
            $rings[] = $points;
        }
        return $rings;
    }

    function shapetype () {
        $ar = unpack('V1V', substr($this->shx_content, 32, 4));
        return $ar['V'];
    }
}

?>