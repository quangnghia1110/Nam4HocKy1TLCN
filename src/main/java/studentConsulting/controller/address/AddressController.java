package studentConsulting.controller.address;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import studentConsulting.model.entity.address.DistrictEntity;
import studentConsulting.model.entity.address.ProvinceEntity;
import studentConsulting.model.entity.address.WardEntity;
import studentConsulting.model.exception.Exceptions.ResourceNotFoundException;
import studentConsulting.model.payload.request.address.DistrictRequest;
import studentConsulting.model.payload.request.address.ProvinceRequest;
import studentConsulting.model.payload.request.address.WardRequest;
import studentConsulting.repository.address.DistrictRepository;
import studentConsulting.repository.address.ProvinceRepository;
import studentConsulting.repository.address.WardRepository;

import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/addresses")
public class AddressController {

    @Autowired
    private ProvinceRepository provinceRepository;
    
    @Autowired
    private DistrictRepository districtRepository;
    
    @Autowired
    private WardRepository wardRepository;

    @GetMapping("/provinces")
    public ResponseEntity<List<ProvinceRequest>> getProvinces() {
        List<ProvinceEntity> provinces = provinceRepository.findAll();
        //Chuyển đổi danh sách provinces (danh sách các đối tượng ProvinceEntity) thành một Stream.
        List<ProvinceRequest> provinceRequests = provinces.stream()
                //Chuyển đổi từng phần tử của Stream từ loại ProvinceEntity sang loại ProvinceRequest.
        		.map(province -> new ProvinceRequest(province.getCode(), province.getName()))
                //Thu thập các phần tử của Stream thành một danh sách
                .collect(Collectors.toList());
        return ResponseEntity.ok(provinceRequests);
    }

    @PostMapping("/provinces")
    public ResponseEntity<ProvinceEntity> createProvince(@RequestBody ProvinceRequest provinceRequest) {
        ProvinceEntity province = new ProvinceEntity();
        province.setCode(provinceRequest.getCode()); // Changed to `code`
        province.setName(provinceRequest.getName());
        ProvinceEntity savedProvince = provinceRepository.save(province);
        return ResponseEntity.ok(savedProvince);
    }

    @PutMapping("/provinces/{code}")
    public ResponseEntity<ProvinceEntity> updateProvince(@PathVariable String code, @RequestBody ProvinceRequest provinceRequest) {
        ProvinceEntity province = provinceRepository.findByCode(code)
                .orElseThrow(() -> new ResourceNotFoundException("Province", "code", code));
        province.setName(provinceRequest.getName());
        ProvinceEntity updatedProvince = provinceRepository.save(province);
        return ResponseEntity.ok(updatedProvince);
    }

    @GetMapping("/districts/{provinceCode}")
    public ResponseEntity<List<DistrictRequest>> getDistrictsByProvince(@PathVariable String provinceCode) {
        List<DistrictEntity> districts = districtRepository.findByProvinceCode(provinceCode);
        List<DistrictRequest> districtRequests = districts.stream()
                .map(district -> new DistrictRequest(district.getCode(), district.getName(), district.getProvince().getCode()))
                .collect(Collectors.toList());
        return ResponseEntity.ok(districtRequests);
    }

    @PostMapping("/districts")
    public ResponseEntity<DistrictEntity> createDistrict(@RequestBody DistrictRequest districtRequest) {
        DistrictEntity district = new DistrictEntity();
        district.setCode(districtRequest.getCode()); // Changed to `code`
        district.setName(districtRequest.getName());
        ProvinceEntity province = provinceRepository.findByCode(districtRequest.getProvinceCode())
                .orElseThrow(() -> new ResourceNotFoundException("Province", "code", districtRequest.getProvinceCode()));
        district.setProvince(province);
        DistrictEntity savedDistrict = districtRepository.save(district);
        return ResponseEntity.ok(savedDistrict);
    }

    @PutMapping("/districts/{code}")
    public ResponseEntity<DistrictEntity> updateDistrict(@PathVariable String code, @RequestBody DistrictRequest districtRequest) {
        DistrictEntity district = districtRepository.findByCode(code)
                .orElseThrow(() -> new ResourceNotFoundException("District", "code", code));
        district.setName(districtRequest.getName());
        ProvinceEntity province = provinceRepository.findByCode(districtRequest.getProvinceCode())
                .orElseThrow(() -> new ResourceNotFoundException("Province", "code", districtRequest.getProvinceCode()));
        district.setProvince(province);
        DistrictEntity updatedDistrict = districtRepository.save(district);
        return ResponseEntity.ok(updatedDistrict);
    }

    @GetMapping("/wards/{districtCode}")
    public ResponseEntity<List<WardRequest>> getWardsByDistrict(@PathVariable String districtCode) {
        List<WardEntity> wards = wardRepository.findByDistrictCode(districtCode);
        List<WardRequest> wardRequests = wards.stream()
                .map(ward -> new WardRequest(ward.getCode(), ward.getName(), ward.getDistrict().getCode()))
                .collect(Collectors.toList());
        return ResponseEntity.ok(wardRequests);
    }

    @PostMapping("/wards")
    public ResponseEntity<WardEntity> createWard(@RequestBody WardRequest wardRequest) {
        WardEntity ward = new WardEntity();
        ward.setCode(wardRequest.getCode()); // Changed to `code`
        ward.setName(wardRequest.getName());
        DistrictEntity district = districtRepository.findByCode(wardRequest.getDistrictCode())
                .orElseThrow(() -> new ResourceNotFoundException("District", "code", wardRequest.getDistrictCode()));
        ward.setDistrict(district);
        WardEntity savedWard = wardRepository.save(ward);
        return ResponseEntity.ok(savedWard);
    }

    @PutMapping("/wards/{code}")
    public ResponseEntity<WardEntity> updateWard(@PathVariable String code, @RequestBody WardRequest wardRequest) {
        WardEntity ward = wardRepository.findByCode(code)
                .orElseThrow(() -> new ResourceNotFoundException("Ward", "code", code));
        ward.setName(wardRequest.getName());
        DistrictEntity district = districtRepository.findByCode(wardRequest.getDistrictCode())
                .orElseThrow(() -> new ResourceNotFoundException("District", "code", wardRequest.getDistrictCode()));
        ward.setDistrict(district);
        WardEntity updatedWard = wardRepository.save(ward);
        return ResponseEntity.ok(updatedWard);
    }
}
