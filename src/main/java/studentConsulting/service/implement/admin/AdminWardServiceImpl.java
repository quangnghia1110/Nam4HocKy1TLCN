package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.DistrictEntity;
import studentConsulting.model.entity.WardEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.manage.ManageWardDTO;
import studentConsulting.model.payload.mapper.admin.WardMapper;
import studentConsulting.model.payload.request.WardRequest;
import studentConsulting.repository.admin.DistrictRepository;
import studentConsulting.repository.admin.WardRepository;
import studentConsulting.service.interfaces.admin.IAdminWardService;
import studentConsulting.specification.admin.WardSpecification;

import java.util.Optional;

@Service
public class AdminWardServiceImpl implements IAdminWardService {

    @Autowired
    private WardRepository wardRepository;

    @Autowired
    private DistrictRepository districtRepository;

    @Autowired
    private WardMapper wardMapper;

    @Override
    @Transactional
    public ManageWardDTO createWard(String code, String districtCode, WardRequest wardRequest) {
        DistrictEntity district = districtRepository.findById(districtCode)
                .orElseThrow(() -> new ErrorException("Không tìm thấy quận/huyện với mã: " + districtCode));

        WardEntity ward = WardEntity.builder()
                .code(code)
                .name(wardRequest.getName())
                .nameEn(wardRequest.getNameEn())
                .fullName(wardRequest.getFullName())
                .fullNameEn(wardRequest.getFullNameEn())
                .codeName(wardRequest.getCodeName())
                .district(district)
                .build();

        WardEntity savedWard = wardRepository.save(ward);

        return wardMapper.mapToDTO(savedWard);
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
        return wardMapper.mapToDTO(updatedWard);
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
                .map(wardMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy Phường/Xã"));
    }

    @Override
    public Page<ManageWardDTO> getWardByAdmin(String code, String name, String nameEn, String fullName,
                                              String fullNameEn, String codeName, String districtCode, Pageable pageable) {
        Specification<WardEntity> spec = Specification.where(null);

        if (code != null && !code.isEmpty()) {
            spec = spec.and(WardSpecification.hasCode(code));
        }

        if (name != null && !name.isEmpty()) {
            spec = spec.and(WardSpecification.hasName(name));
        }

        if (nameEn != null && !nameEn.isEmpty()) {
            spec = spec.and(WardSpecification.hasNameEn(nameEn));
        }

        if (fullName != null && !fullName.isEmpty()) {
            spec = spec.and(WardSpecification.hasFullName(fullName));
        }

        if (fullNameEn != null && !fullNameEn.isEmpty()) {
            spec = spec.and(WardSpecification.hasFullNameEn(fullNameEn));
        }

        if (codeName != null && !codeName.isEmpty()) {
            spec = spec.and(WardSpecification.hasCodeName(codeName));
        }

        if (districtCode != null && !districtCode.isEmpty()) {
            spec = spec.and(WardSpecification.hasDistrictCode(districtCode));
        }

        return wardRepository.findAll(spec, pageable).map(wardMapper::mapToDTO);
    }


    @Override
    public boolean existsByCode(String code) {
        return wardRepository.existsByCode(code);
    }

}
