package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.address.ProvinceEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.address.ManageProvinceDTO;
import studentConsulting.model.payload.request.address.ProvinceRequest;
import studentConsulting.repository.address.ProvinceRepository;
import studentConsulting.service.interfaces.admin.IAdminProvinceService;
import studentConsulting.specification.address.ProvinceSpecification;

import java.util.Optional;

@Service
public class AdminProvinceServiceImpl implements IAdminProvinceService {

    @Autowired
    private ProvinceRepository provinceRepository;

    private ManageProvinceDTO mapToDTO(ProvinceEntity province) {
        return ManageProvinceDTO.builder()
                .code(province.getCode())
                .name(province.getName())
                .nameEn(province.getNameEn())
                .fullName(province.getFullName())
                .fullNameEn(province.getFullNameEn())
                .codeName(province.getCodeName())
                .build();
    }

    private ProvinceEntity mapToEntity(ProvinceRequest provinceRequest, String code) {
        return ProvinceEntity.builder()
                .code(code)
                .name(provinceRequest.getName())
                .nameEn(provinceRequest.getNameEn())
                .fullName(provinceRequest.getFullName())
                .fullNameEn(provinceRequest.getFullNameEn())
                .codeName(provinceRequest.getCodeName())
                .build();
    }

    @Override
    @Transactional
    public ManageProvinceDTO createProvince(ProvinceRequest provinceRequest, String code) {
        if (provinceRepository.existsByCode(code)) {
            throw new ErrorException("Mã tỉnh đã tồn tại: " + code);
        }

        ProvinceEntity province = mapToEntity(provinceRequest, code);
        ProvinceEntity savedProvince = provinceRepository.save(province);
        return mapToDTO(savedProvince);
    }


    @Override
    @Transactional
    public ManageProvinceDTO updateProvince(String code, ProvinceRequest provinceRequest) {
        ProvinceEntity existingProvince = provinceRepository.findById(code)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + code));

        existingProvince.setName(Optional.ofNullable(provinceRequest.getName()).orElse(existingProvince.getName()));
        existingProvince.setNameEn(Optional.ofNullable(provinceRequest.getNameEn()).orElse(existingProvince.getNameEn()));
        existingProvince.setFullName(Optional.ofNullable(provinceRequest.getFullName()).orElse(existingProvince.getFullName()));
        existingProvince.setFullNameEn(Optional.ofNullable(provinceRequest.getFullNameEn()).orElse(existingProvince.getFullNameEn()));
        existingProvince.setCodeName(Optional.ofNullable(provinceRequest.getCodeName()).orElse(existingProvince.getCodeName()));

        ProvinceEntity updatedProvince = provinceRepository.save(existingProvince);
        return mapToDTO(updatedProvince);
    }

    @Override
    @Transactional
    public void deleteProvinceByCode(String code) {
        ProvinceEntity province = provinceRepository.findById(code)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + code));
        provinceRepository.delete(province);
    }

    @Override
    public ManageProvinceDTO getProvinceByCode(String code) {
        return provinceRepository.findById(code)
                .map(this::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + code));
    }

    @Override
    public Page<ManageProvinceDTO> getAllProvincesWithFilters(Optional<String> code, Optional<String> name, Optional<String> nameEn, Optional<String> fullName, Optional<String> fullNameEn, Optional<String> codeName, Pageable pageable) {
        Specification<ProvinceEntity> spec = Specification.where(null);

        if (code.isPresent()) {
            spec = spec.and(ProvinceSpecification.hasCode(code.get()));
        }
        if (name.isPresent()) {
            spec = spec.and(ProvinceSpecification.hasName(name.get()));
        }
        if (nameEn.isPresent()) {
            spec = spec.and(ProvinceSpecification.hasNameEn(nameEn.get()));
        }
        if (fullName.isPresent()) {
            spec = spec.and(ProvinceSpecification.hasFullName(fullName.get()));
        }
        if (fullNameEn.isPresent()) {
            spec = spec.and(ProvinceSpecification.hasFullNameEn(fullNameEn.get()));
        }
        if (codeName.isPresent()) {
            spec = spec.and(ProvinceSpecification.hasCodeName(codeName.get()));
        }

        return provinceRepository.findAll(spec, pageable)
                .map(this::mapToDTO);
    }

    @Override
    public boolean existsByCode(String code) {
        return provinceRepository.existsByCode(code);
    }
}
