package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.department_field.FieldEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.department_field.ManageFieldDTO;
import studentConsulting.model.payload.request.department_field.FieldRequest;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.department_field.FieldRepository;
import studentConsulting.service.interfaces.admin.IAdminFieldService;
import studentConsulting.specification.department_field.FieldSpecification;

import java.time.LocalDate;
import java.util.Optional;

@Service
public class AdminFieldServiceImpl implements IAdminFieldService {

    @Autowired
    private FieldRepository fieldRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    private ManageFieldDTO mapToDTO(FieldEntity field) {
        return ManageFieldDTO.builder()
                .id(field.getId())
                .createdAt(field.getCreatedAt())
                .name(field.getName())
                .departmentId(field.getDepartment() != null ? field.getDepartment().getId() : null)
                .build();
    }

    private FieldEntity mapToEntity(FieldRequest fieldRequest, DepartmentEntity department) {
        return FieldEntity.builder()
                .name(fieldRequest.getName())
                .department(department)
                .createdAt(LocalDate.now())
                .build();
    }

    @Override
    public ManageFieldDTO createField(Integer departmentId, FieldRequest fieldRequest) {
        DepartmentEntity department = departmentRepository.findById(departmentId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với ID: " + departmentId));

        FieldEntity field = mapToEntity(fieldRequest, department);
        FieldEntity savedField = fieldRepository.save(field);
        return mapToDTO(savedField);
    }

    @Override
    public ManageFieldDTO updateField(Integer id, Integer departmentId, FieldRequest fieldRequest) {
        FieldEntity existingField = fieldRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy lĩnh vực với ID: " + id));

        DepartmentEntity department = departmentRepository.findById(departmentId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với ID: " + departmentId));

        existingField.setName(fieldRequest.getName());
        existingField.setDepartment(department);
        FieldEntity updatedField = fieldRepository.save(existingField);
        return mapToDTO(updatedField);
    }


    @Override
    @Transactional
    public void deleteFieldById(Integer id) {
        FieldEntity field = fieldRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy lĩnh vực với ID: " + id));
        fieldRepository.delete(field);
    }

    @Override
    public ManageFieldDTO getFieldById(Integer id) {
        return fieldRepository.findById(id)
                .map(this::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy lĩnh vực với ID: " + id));
    }

    public Page<ManageFieldDTO> getAllFieldsWithFilters(Optional<String> name, Optional<String> departmentId, Pageable pageable) {
        Specification<FieldEntity> spec = Specification.where(null);

        if (name.isPresent()) {
            spec = spec.and(FieldSpecification.hasName(name.get()));
        }

        if (departmentId.isPresent()) {
            spec = spec.and(FieldSpecification.hasDepartmentId(departmentId.get()));
        }

        return fieldRepository.findAll(spec, pageable)
                .map(this::mapToDTO);
    }

    @Override
    public boolean existsById(Integer id) {
        return fieldRepository.existsById(id);
    }
}
