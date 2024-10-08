package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.department_field.ImportDepartmentDTO;
import studentConsulting.model.payload.dto.department_field.ManageDepartmentDTO;
import studentConsulting.model.payload.request.department_field.DepartmentRequest;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.service.interfaces.admin.IAdminDepartmentService;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdminDepartmentServiceImpl implements IAdminDepartmentService {

    @Autowired
    private DepartmentRepository departmentRepository;

    private ManageDepartmentDTO mapToDTO(DepartmentEntity department) {
        return ManageDepartmentDTO.builder()
                .id(department.getId())
                .createdAt(department.getCreatedAt())
                .name(department.getName())
                .description(department.getDescription())
                .logo(department.getLogo())
                .build();
    }

    private DepartmentEntity mapToEntity(DepartmentRequest departmentRequest) {
        return DepartmentEntity.builder()
                .name(departmentRequest.getName())
                .description(departmentRequest.getDescription())
                .logo(departmentRequest.getLogo())
                .createdAt(LocalDate.now())
                .build();
    }

    @Override
    @Transactional
    public ManageDepartmentDTO createDepartment(DepartmentRequest departmentRequest) {
        DepartmentEntity department = mapToEntity(departmentRequest);
        DepartmentEntity savedDepartment = departmentRepository.save(department);
        return mapToDTO(savedDepartment);
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
        return mapToDTO(updatedDepartment);
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
                .map(this::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với ID: " + id));
    }

    @Override
    public Page<ManageDepartmentDTO> getAllDepartmentsWithFilters(Optional<String> name, Pageable pageable) {
        return departmentRepository.findAllByNameContaining(name.orElse(""), pageable)
                .map(this::mapToDTO);
    }

    @Override
    public boolean existsById(Integer id) {
        return departmentRepository.existsById(id);
    }

    @Override
    public void importDepartments(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        List<ImportDepartmentDTO> departments = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        LocalDate createdAt = LocalDate.parse(row.get(1), formatter);  // Chuyển đổi thành LocalDate
                        String description = row.get(2);
                        String logo = row.get(3);
                        String name = row.get(4);

                        return new ImportDepartmentDTO(id, createdAt, description, logo, name);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Department: ");
                    }
                })
                .collect(Collectors.toList());

        departments.forEach(department -> {
            try {
                DepartmentEntity entity = new DepartmentEntity();
                entity.setId(department.getId());
                entity.setCreatedAt(department.getCreatedAt());
                entity.setDescription(department.getDescription());
                entity.setLogo(department.getLogo());
                entity.setName(department.getName());

                departmentRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Department vào database: ");
            }
        });
    }

}

