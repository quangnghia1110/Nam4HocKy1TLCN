package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.DepartmentEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.manage.ManageDepartmentDTO;
import studentConsulting.model.payload.mapper.admin.DepartmentMapper;
import studentConsulting.model.payload.request.DepartmentRequest;
import studentConsulting.repository.admin.DepartmentRepository;
import studentConsulting.service.interfaces.admin.IAdminDepartmentService;

import java.time.LocalDate;

@Service
public class AdminDepartmentServiceImpl implements IAdminDepartmentService {

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private DepartmentMapper departmentMapper;

    @Override
    @Transactional
    public ManageDepartmentDTO createDepartment(DepartmentRequest departmentRequest) {
        DepartmentEntity department = DepartmentEntity.builder()
                .name(departmentRequest.getName())
                .description(departmentRequest.getDescription())
                .logo(departmentRequest.getLogo())
                .createdAt(LocalDate.now())
                .build();

        DepartmentEntity savedDepartment = departmentRepository.save(department);

        return departmentMapper.mapToDTO(savedDepartment);
    }


    @Override
    @Transactional
    public ManageDepartmentDTO updateDepartment(Integer id, DepartmentRequest departmentRequest) {
        DepartmentEntity existingDepartment = departmentRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với ID: " + id));

        existingDepartment.setName(departmentRequest.getName());
        existingDepartment.setDescription(departmentRequest.getDescription());
        existingDepartment.setLogo(departmentRequest.getLogo());

        DepartmentEntity updatedDepartment = departmentRepository.save(existingDepartment);
        return departmentMapper.mapToDTO(updatedDepartment);
    }

    @Override
    @Transactional
    public void deleteDepartmentById(Integer id) {
        DepartmentEntity department = departmentRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với ID: " + id));
        departmentRepository.delete(department);
    }

    @Override
    public ManageDepartmentDTO getDepartmentById(Integer id) {
        return departmentRepository.findById(id)
                .map(departmentMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với ID: " + id));
    }

    @Override
    public Page<ManageDepartmentDTO> getDepartmentByAdmin(String name, Pageable pageable) {
        return departmentRepository.findAllByNameContaining((name != null) ? name : "", pageable)
                .map(departmentMapper::mapToDTO);
    }

    @Override
    public boolean existsById(Integer id) {
        return departmentRepository.existsById(id);
    }
}

