#!/usr/bin/perl

# Converts a .3ds file to a .gay mesh

my %id = {
    'MAIN3DS' => 0x4d4d,
    'MAT_ENTRY' => 0xafff
};

open(INPUT,$ARGV[0]);
binmode(INPUT);
open(OUTPUT,">".$ARGV[1]);

my $inside_material = 0;
my $inside_mesh = 0;
my $cur_name = '';
my $cur_faces_num = 0;
my $input_length = (stat(INPUT))[7];
my %cur_map;
my $inside_map = 0;

while (tell INPUT < $input_length) {
    my $chunk_id = readUShort();
    my $chunk_length = readUInt();
    if ($chunk_id == 0x4d4d) {
	print "MAIN3DS\n";
    } elsif ($chunk_id == 0x3d3d) {
	print "EDIT3DS\n";
    } elsif ($chunk_id == 0x4000) {
	if ($inside_material) {
	    if ($inside_map) {
		printCurMap();
		$inside_map = 0;
	    }
	    print OUTPUT "}\n\n";
	    $inside_material = 0;
	}
	print "NAMED_OBJECT\n";
	$name = readString();
	print "   NAME='$name'\n";
	$cur_name = $name;
    } elsif ($chunk_id == 0x4100) {
	if ($inside_mesh) {
	    print OUTPUT "}\n\n";
	    $inside_mesh = 0;
	}
	print OUTPUT "/* $cur_name */\n";
	print OUTPUT "mesh {\n";
	$inside_mesh = 1;
    } elsif ($chunk_id == 0x4110) {
	print "POINT_ARRAY\n";
	my $num = readUShort();
	print "   Vertices: $num\n";
	print OUTPUT "   'vertices (\n";
	for(my $i = 0; $i < $num; $i++) {
	    my @f = (readFloat(),readFloat(),readFloat());
	    printf OUTPUT "      (%.6f %.6f %.6f)\n",@f;
	}
	print OUTPUT "   )\n";
    } elsif ($chunk_id == 0x4120) {
	print "FACE ARRAY\n";
	my $num = readUShort();
	print "   Faces: $num\n";
	print OUTPUT "   'faces (\n";
	for(my $i = 0; $i < $num; $i++) {
	    print OUTPUT "      (".readUShort().' '.readUShort().' '.readUShort().")\n";
	    my $face_flags = readUShort();
	}
	$cur_faces_num = $num;
	print OUTPUT "   }\n";
    } elsif ($chunk_id == 0x4130) {
	my $mat_name = readString();
	my $num = readUShort();
	for(my $i = 0; $i < $num; $i++) {
	    readUShort(); # Refers to faces
	}
	my $var_mat = makeVarName($mat_name);
	print OUTPUT "  ; $num/$cur_faces_num triangles in mesh uses material '$var_mat'\n";
    } elsif ($chunk_id == 0x4140) {
	print "TEX_VERTS\n";
	my $num = readUShort();
	print OUTPUT "   'uv_coords (\n";
	for(my $i = 0; $i < $num; $i++) {
	    printf OUTPUT "      #(%.6f %.6f)\n",readFloat(),readFloat();
	}
	print OUTPUT "   )\n";
    } elsif ($chunk_id == 0xafff) {
	if ($inside_material) {
	    if ($inside_map) {
		printCurMap();
		$inside_map = 0;
	    }
	    print OUTPUT "}\n\n";
	    $inside_material = 0;
	}
	print "MAT_ENTRY\n";
	$inside_material = 1;
    } elsif ($chunk_id == 0xa000) {
	my $name = readString();
	my $varname = makeVarName($name);
	print "MAT_NAME  name='$name'\n";
	print OUTPUT "$varname = material {\n";
	print OUTPUT "    /* Material named '$name' */\n";
    } elsif ($chunk_id == 0xa010) {
	my $color = readColor24();
	print OUTPUT "    /* ambient $color */\n";
    } elsif ($chunk_id == 0xa020) {
	my $color = readColor24();
	print OUTPUT "    diffuse $color\n";
    } elsif ($chunk_id == 0xa030) {
	my $color = readColor24();
	print OUTPUT "    specular $color\n";
    } elsif ($chunk_id == 0xa040) {
	my $amount = readAmountOf();
	print OUTPUT "    /* shininess $amount */\n";
    } elsif ($chunk_id == 0xa041) {
	my $amount = readAmountOf();
	print OUTPUT "    /* shininess strength $amount */\n";
    } elsif ($chunk_id == 0xa050) {
	my $amount = readAmountOf();
	print OUTPUT "    /* transparency $amount */\n";
    } elsif ($chunk_id == 0xa052) {
	my $amount = readAmountOf();
	print OUTPUT "    /* trans. falloff $amount */\n";
    } elsif ($chunk_id == 0xa053) {
	my $amount = readAmountOf();
	print OUTPUT "    /* reflect blur $amount */\n";
    } elsif ($chunk_id == 0xa087) {
	my $amount = readFloat();
	print OUTPUT "    /* wire thickness $amount */\n";
    } elsif ($chunk_id == 0xa100) {
	my $amount = readUShort();
	print OUTPUT "    /* material shading $amount ";
	print OUTPUT " (1=flat 2=gour. 3=phong 4=metal) */\n";
    } elsif ($chunk_id == 0xa200) {
	print "MAT_TEXMAP\n";
	if ($inside_map) {
	    printCurMap();
	    $inside_map = 0;
	}
	%cur_map = { 'MAPTYPE' => 'diffuse'};
	$inside_map = 1;
    } elsif ($chunk_id == 0xa230) {
	print "MAT_BUMPMAP\n";
	if ($inside_map) {
	    printCurMap();
	    $inside_map = 0;
	}
	my $amount = readAmountOf();
	%cur_map = {};
	$cur_map{'MAPTYPE'} = 'bump';
	$cur_map{'BUMPSIZE'} = $amount;
	$inside_map = 1;
    } elsif ($chunk_id == 0xa300) {
	$cur_map{'FILENAME'} = readString();
    } elsif ($chunk_id == 0xa354) {
	$cur_map{'USCALE'} = readFloat();
    } elsif ($chunk_id == 0xa356) {
	$cur_map{'VSCALE'} = readFloat();
    } else {
	printf "%04x size=%d\n",$chunk_id,$chunk_length;
	seek(INPUT,$chunk_length-6,1);
    }
}
if ($inside_mesh) {
    print OUTPUT "}\n";
}

sub printCurMap {
    print OUTPUT "   ";
    if ($cur_map{'MAPTYPE'} eq 'bump') {
	print OUTPUT 'bump '.$cur_map{'BUMPSIZE'}.' ';
    } else {
	print OUTPUT "diffuse ";
    }
    print OUTPUT "texture { ";
    print OUTPUT '"'.lc($cur_map{'FILENAME'}).'" ';
    $uscale = $cur_map{'USCALE'} || 1;
    $vscale = $cur_map{'VSCALE'} || 1;
    print OUTPUT "$uscale $vscale bilinear }\n";
}

sub readByte {
    my $b;
    read(INPUT,$b,1);
    return unpack ('C',$b);
}

sub readUShort {
    my $b;
    read(INPUT,$b,2);
    return unpack ('S',$b);
}

sub readUInt {
    my $b;
    read(INPUT,$b,4);
    return unpack ('I',$b);
}

sub readFloat {
    my $b;
    read(INPUT,$b,4);
    return unpack ('f',$b);
}

sub readString {
    my $result;
    local $/ = "\0";
    seek(INPUT,$result,1);
    $result = <INPUT>;
    chomp $result;
    return $result;
}

sub readColor24 {
    my $id = readUShort();
    my $len  = readUInt();
    print "************* WARNING! ***********\n" if ($id != 0x11);
    my @col = (readByte()/255.0,readByte()/255.0,readByte()/255.0);
    
    return sprintf "#(%.5f %.5f %.5f)",@col;
}

sub readAmountOf {
    my $id = readUShort();
    my $len  = readUInt();
    print "************* WARNING! ***********\n" if ($id != 0x30);
    return readUShort();
}

sub makeVarName {
    my $name = shift;
    $name =~ s/ /_/g;
    return '$'.lc($name);
}
