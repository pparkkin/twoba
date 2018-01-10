provider "aws" {
  region = "${var.region}"
}

## IAM Stuffs

resource "aws_iam_role" "twoba_instance" {
  name = "twoba_instance"
  assume_role_policy = "${file("twoba-instance-role.json")}"
}

resource "aws_iam_role_policy" "twoba_instance" { 
  name = "twoba_instance"
  role = "${aws_iam_role.twoba_instance.id}"
  policy = "${file("twoba-instance-policy.json")}"
}

## Security Stuffs

resource "aws_key_pair" "treehouse" {
  key_name = "paavo_key"
  public_key = "${var.public_key}"
}

resource "aws_security_group" "allow_ssh" {
  name = "allow_ssh"

  ingress {
    from_port = 22
    to_port = 22
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "allow_vnc" {
  name = "allow_vnc"

  ingress {
    from_port = 5900
    to_port = 5950
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "http_out" {
  name = "http_out"

  egress {
    from_port = 80
    to_port = 80
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  egress {
    from_port = 443
    to_port = 443
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "allow_twoba" {
  name = "allow_twoba"

  ingress {
    from_port = 33000
    to_port = 33000
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port = 33000
    to_port = 33000
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

}

## Instances

resource "aws_iam_instance_profile" "twoba_instance" {
  name = "twoba_instance_profile"
  role = "${aws_iam_role.twoba_instance.name}"
}

data "template_file" "ecs_config" {
  template = "${file("ecs.config.tpl")}"
  vars {
    cluster_name = "${aws_ecs_cluster.twoba.name}"
  }
}

resource "aws_instance" "ecs_instance" {
  ami = "${lookup(var.amis, var.region)}"
  instance_type = "t2.micro"
  security_groups = [
    "${aws_security_group.allow_ssh.name}",
    "${aws_security_group.http_out.name}",
    "${aws_security_group.allow_twoba.name}"
  ]
  key_name = "${aws_key_pair.treehouse.key_name}"
  iam_instance_profile = "${aws_iam_instance_profile.twoba_instance.name}"
  user_data = "${data.template_file.ecs_config.rendered}"
}

output "twoba-name" {
  value = "${aws_instance.ecs_instance.public_dns}"
}


## ECS

resource "aws_ecs_cluster" "twoba" {
  name = "twoba_cluster"
}

resource "aws_ecs_task_definition" "twoba" {
  family = "twoba_task"
  container_definitions = "${file("ecs-task.json")}"
}

resource "aws_ecs_service" "twoba" {
  name = "twoba_service"
  task_definition = "${aws_ecs_task_definition.twoba.arn}"
  cluster = "${aws_ecs_cluster.twoba.id}"
  desired_count = 1
}

## Ubuntu Desktop (for testing)

resource "aws_instance" "desktop" {
  ami = "ami-5055cd3f"
  instance_type = "t2.medium"
  security_groups = [
    "${aws_security_group.allow_ssh.name}",
    "${aws_security_group.allow_vnc.name}",
    "${aws_security_group.http_out.name}",
    "${aws_security_group.allow_twoba.name}"
  ]
  key_name = "${aws_key_pair.treehouse.key_name}"
}

# $ sudo apt-get update && sudo apt-get install --no-install-recommends lubuntu-desktop
# $ sudo apt-get install tightvncserver
# $ vncserver # and configure
output "desktop-name" {
  value = "${aws_instance.desktop.public_dns}"
}

