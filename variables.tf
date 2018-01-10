variable "region" {
  default = "eu-central-1"
}

# ECS optimized instances
variable "amis" {
  type = "map"
  default = {
    "eu-central-1" = "ami-509a053f"
  }
}

variable "public_key" { }
