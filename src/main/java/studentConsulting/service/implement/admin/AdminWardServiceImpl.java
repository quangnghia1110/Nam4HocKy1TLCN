package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.address.DistrictEntity;
import studentConsulting.model.entity.address.WardEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.address.ManageWardDTO;
import studentConsulting.model.payload.request.address.WardRequest;
import studentConsulting.repository.address.DistrictRepository;
import studentConsulting.repository.address.WardRepository;
import studentConsulting.service.interfaces.admin.IAdminWardService;
import studentConsulting.specification.address.WardSpecification;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdminWardServiceImpl implements IAdminWardService {

    @Autowired
    private WardRepository wardRepository;

    @Autowired
    private DistrictRepository districtRepository;

    private ManageWardDTO mapToDTO(WardEntity ward) {
        return ManageWardDTO.builder()
                .code(ward.getCode())
                .name(ward.getName())
                .nameEn(ward.getNameEn())
                .fullName(ward.getFullName())
                .fullNameEn(ward.getFullNameEn())
                .codeName(ward.getCodeName())
                .districtCode(ward.getDistrict() != null ? ward.getDistrict().getCode() : null)
                .build();
    }

    private WardEntity mapToEntity(WardRequest wardRequest, String code, String districtCode) {
        DistrictEntity district = districtRepository.findById(districtCode)
                .orElseThrow(() -> new ErrorException("Không tìm thấy quận/huyện với mã: " + districtCode));

        return WardEntity.builder()
                .code(code)
                .name(wardRequest.getName())
                .nameEn(wardRequest.getNameEn())
                .fullName(wardRequest.getFullName())
                .fullNameEn(wardRequest.getFullNameEn())
                .codeName(wardRequest.getCodeName())
                .district(district)
                .build();
    }

    @Override
    @Transactional
    public ManageWardDTO createWard(String code, String districtCode, WardRequest wardRequest) {
        WardEntity ward = mapToEntity(wardRequest, code, districtCode);
        WardEntity savedWard = wardRepository.save(ward);
        return mapToDTO(savedWard);
    }

    @Override
    @Transactional
    public ManageWardDTO updateWard(String code, String districtCode, WardRequest wardRequest) {
        WardEntity existingWard = wardRepository.findById(code)
                .orElseThrow(() -> new ErrorException("Phường/Xã không tồn tại"));

        existingWard.setName(Optional.ofNullable(wardRequest.getName()).orElse(existingWard.getName()));
        existingWard.setNameEn(Optional.ofNullable(wardRequest.getNameEn()).orElse(existingWard.getNameEn()));
        existingWard.setFullName(Optional.ofNullable(wardRequest.getFullName()).orElse(existingWard.getFullName()));
        existingWard.setFullNameEn(Optional.ofNullable(wardRequest.getFullNameEn()).orElse(existingWard.getFullNameEn()));
        existingWard.setCodeName(Optional.ofNullable(wardRequest.getCodeName()).orElse(existingWard.getCodeName()));

        if (districtCode != null) {
            DistrictEntity district = districtRepository.findById(districtCode)
                    .orElseThrow(() -> new ErrorException("Không tìm thấy quận/huyện với mã: " + districtCode));
            existingWard.setDistrict(district);
        }

        WardEntity updatedWard = wardRepository.save(existingWard);
        return mapToDTO(updatedWard);
    }

    @Override
    @Transactional
    public void deleteWardByCode(String code) {
        WardEntity ward = wardRepository.findById(code)
                .orElseThrow(() -> new ErrorException("Phường/Xã không tồn tại"));
        wardRepository.delete(ward);
    }

    @Override
    public ManageWardDTO getWardByCode(String code) {
        return wardRepository.findById(code)
                .map(this::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy Phường/Xã"));
    }

    @Override
    public Page<ManageWardDTO> getAllWardsWithFilters(Optional<String> code, Optional<String> name, Optional<String> nameEn, Optional<String> fullName, Optional<String> fullNameEn, Optional<String> codeName, Optional<String> districtCode, Pageable pageable) {
        Specification<WardEntity> spec = Specification.where(null);

        if (code.isPresent()) {
            spec = spec.and(WardSpecification.hasCode(code.get()));
        }

        if (name.isPresent()) {
            spec = spec.and(WardSpecification.hasName(name.get()));
        }

        if (nameEn.isPresent()) {
            spec = spec.and(WardSpecification.hasNameEn(nameEn.get()));
        }

        if (fullName.isPresent()) {
            spec = spec.and(WardSpecification.hasFullName(fullName.get()));
        }

        if (fullNameEn.isPresent()) {
            spec = spec.and(WardSpecification.hasFullNameEn(fullNameEn.get()));
        }

        if (codeName.isPresent()) {
            spec = spec.and(WardSpecification.hasCodeName(codeName.get()));
        }

        if (districtCode.isPresent()) {
            spec = spec.and(WardSpecification.hasDistrictCode(districtCode.get()));
        }

        return wardRepository.findAll(spec, pageable).map(this::mapToDTO);
    }

    @Override
    public boolean existsByCode(String code) {
        return wardRepository.existsByCode(code);
    }

    @Override
    public void importWards(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ManageWardDTO> wards = filteredData.stream()
                .map(row -> {
                    try {
                        String code = row.get(0);
                        String name = row.get(1);
                        String nameEn = row.get(2);
                        String fullName = row.get(3);
                        String fullNameEn = row.get(4);
                        String codeName = row.get(5);
                        String districtCode = row.get(6);

                        return new ManageWardDTO(code, name, nameEn, fullName, fullNameEn, codeName, districtCode);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Ward: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        wards.forEach(ward -> {
            try {
                WardEntity entity = new WardEntity();
                entity.setCode(ward.getCode());
                entity.setName(ward.getName());
                entity.setNameEn(ward.getNameEn());
                entity.setFullName(ward.getFullName());
                entity.setFullNameEn(ward.getFullNameEn());
                entity.setCodeName(ward.getCodeName());

                // Gán quận/huyện từ districtCode
                DistrictEntity district = districtRepository.findByCode(ward.getDistrictCode())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy quận/huyện với mã: " + ward.getDistrictCode()));
                entity.setDistrict(district);

                wardRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Ward vào database: " + e.getMessage());
            }
        });
    }

}
