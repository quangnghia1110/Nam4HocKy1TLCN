package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.address.DistrictEntity;
import studentConsulting.model.entity.address.ProvinceEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.address.ManageDistrictDTO;
import studentConsulting.model.payload.mapper.admin.DistrictMapper;
import studentConsulting.model.payload.request.address.DistrictRequest;
import studentConsulting.repository.address.DistrictRepository;
import studentConsulting.repository.address.ProvinceRepository;
import studentConsulting.service.interfaces.admin.IAdminDistrictService;
import studentConsulting.specification.address.DistrictSpecification;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdminDistrictServiceImpl implements IAdminDistrictService {

    @Autowired
    private DistrictRepository districtRepository;

    @Autowired
    private ProvinceRepository provinceRepository;

    @Autowired
    private DistrictMapper districtMapper;

    @Transactional
    public ManageDistrictDTO createDistrict(String code, String provinceCode, DistrictRequest districtRequest) {
        ProvinceEntity province = provinceRepository.findById(provinceCode)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + provinceCode));

        DistrictEntity district = DistrictEntity.builder()
                .code(code)
                .name(districtRequest.getName())
                .nameEn(districtRequest.getNameEn())
                .fullName(districtRequest.getFullName())
                .fullNameEn(districtRequest.getFullNameEn())
                .codeName(districtRequest.getCodeName())
                .province(province)
                .build();

        DistrictEntity savedDistrict = districtRepository.save(district);

        return districtMapper.mapToDTO(savedDistrict);
    }


    @Transactional
    public ManageDistrictDTO updateDistrict(String code, String provinceCode, DistrictRequest districtRequest) {
        DistrictEntity existingDistrict = districtRepository.findById(code)
                .orElseThrow(() -> new ErrorException("Quận/Huyện không tồn tại"));

        existingDistrict.setName(Optional.ofNullable(districtRequest.getName()).orElse(existingDistrict.getName()));
        existingDistrict.setNameEn(Optional.ofNullable(districtRequest.getNameEn()).orElse(existingDistrict.getNameEn()));
        existingDistrict.setFullName(Optional.ofNullable(districtRequest.getFullName()).orElse(existingDistrict.getFullName()));
        existingDistrict.setFullNameEn(Optional.ofNullable(districtRequest.getFullNameEn()).orElse(existingDistrict.getFullNameEn()));
        existingDistrict.setCodeName(Optional.ofNullable(districtRequest.getCodeName()).orElse(existingDistrict.getCodeName()));

        if (provinceCode != null) {
            ProvinceEntity province = provinceRepository.findById(provinceCode)
                    .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + provinceCode));
            existingDistrict.setProvince(province);
        }

        DistrictEntity updatedDistrict = districtRepository.save(existingDistrict);
        return districtMapper.mapToDTO(updatedDistrict);
    }

    @Transactional
    public void deleteDistrictByCode(String code) {
        DistrictEntity district = districtRepository.findById(code)
                .orElseThrow(() -> new ErrorException("Quận/Huyện không tồn tại"));
        districtRepository.delete(district);
    }

    public ManageDistrictDTO getDistrictByCode(String code) {
        return districtRepository.findById(code)
                .map(districtMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy Quận/Huyện"));
    }

    public Page<ManageDistrictDTO> getAllDistrictsWithFilters(String code, String name, String nameEn, String fullName,
                                                              String fullNameEn, String codeName, String provinceCode, Pageable pageable) {
        Specification<DistrictEntity> spec = Specification.where(null);

        if (code != null && !code.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasCode(code));
        }

        if (name != null && !name.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasName(name));
        }

        if (nameEn != null && !nameEn.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasNameEn(nameEn));
        }

        if (fullName != null && !fullName.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasFullName(fullName));
        }

        if (fullNameEn != null && !fullNameEn.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasFullNameEn(fullNameEn));
        }

        if (codeName != null && !codeName.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasCodeName(codeName));
        }

        if (provinceCode != null && !provinceCode.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasProvinceCode(provinceCode));
        }

        return districtRepository.findAll(spec, pageable)
                .map(districtMapper::mapToDTO);
    }


    public boolean existsByCode(String code) {
        return districtRepository.existsByCode(code);
    }

    @Override
    public void importDistricts(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ManageDistrictDTO> districts = filteredData.stream()
                .map(row -> {
                    try {
                        String code = row.get(0);
                        String name = row.get(1);
                        String nameEn = row.get(2);
                        String fullName = row.get(3);
                        String fullNameEn = row.get(4);
                        String codeName = row.get(5);
                        String provinceCode = row.get(6);

                        return new ManageDistrictDTO(code, name, nameEn, fullName, fullNameEn, codeName, provinceCode);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu District: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        districts.forEach(district -> {
            try {
                DistrictEntity entity = new DistrictEntity();
                entity.setCode(district.getCode());
                entity.setName(district.getName());
                entity.setNameEn(district.getNameEn());
                entity.setFullName(district.getFullName());
                entity.setFullNameEn(district.getFullNameEn());
                entity.setCodeName(district.getCodeName());

                ProvinceEntity province = provinceRepository.findByCode(district.getProvinceCode())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy tỉnh với mã: " + district.getProvinceCode()));
                entity.setProvince(province);

                districtRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu District vào database: " + e.getMessage());
            }
        });
    }

}
