terraform {
  required_providers {
    tencentcloud = {
      source = "tencentcloudstack/tencentcloud"
    }
  }
}

provider "tencentcloud" {
  region     = "ap-hongkong"
}

data "tencentcloud_availability_zones_by_product" "default" {
  product = "cvm"
}

data "tencentcloud_images" "default" {
  image_type = ["PUBLIC_IMAGE"]
  os_name    = "ubuntu"
}

data "tencentcloud_instance_types" "default" {
  cpu_core_count = 2
  memory_size    = 2
}

resource "tencentcloud_instance" "ubuntu" {
  instance_name              = "ubuntu"
  availability_zone          = data.tencentcloud_availability_zones_by_product.default.zones.0.name
  image_id                   = data.tencentcloud_images.default.images.0.image_id
  instance_type              = data.tencentcloud_instance_types.default.instance_types.0.instance_type
  instance_charge_type       = "PREPAID"
  instance_charge_type_prepaid_period = 1
  system_disk_type           = "CLOUD_PREMIUM"
  system_disk_size           = 50
  allocate_public_ip         = true
  orderly_security_groups    = ["sg-kyu3mynt"]
  internet_charge_type       = "TRAFFIC_POSTPAID_BY_HOUR"
  internet_max_bandwidth_out = 50
  key_ids		     = ["skey-15yu6stt"]
  count                      = 1
}
