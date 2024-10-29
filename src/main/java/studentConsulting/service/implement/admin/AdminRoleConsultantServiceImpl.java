package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.authentication.RoleEntity;
import studentConsulting.model.entity.user.RoleConsultantEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.user.ManageRoleConsultantDTO;
import studentConsulting.model.payload.mapper.admin.RoleConsultantMapper;
import studentConsulting.model.payload.request.authentication.RoleConsultantRequest;
import studentConsulting.repository.authentication.RoleRepository;
import studentConsulting.repository.user.RoleConsultantRepository;
import studentConsulting.service.interfaces.admin.IAdminRoleConsultantService;
import studentConsulting.specification.authentication.RoleConsultantSpecification;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdminRoleConsultantServiceImpl implements IAdminRoleConsultantService {

    @Autowired
    private RoleConsultantRepository roleConsultantRepository;

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    private RoleConsultantMapper roleConsultantMapper;

    @Override
    @Transactional
    public ManageRoleConsultantDTO createRoleConsultant(Integer roleId, RoleConsultantRequest roleConsultantRequest) {
        RoleEntity role = roleRepository.findById(roleId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với ID: " + roleId));

        RoleConsultantEntity roleConsultant = RoleConsultantEntity.builder()
                .name(roleConsultantRequest.getName())
                .role(role)
                .createdAt(LocalDate.now())
                .build();

        RoleConsultantEntity savedRoleConsultant = roleConsultantRepository.save(roleConsultant);

        return roleConsultantMapper.mapToDTO(savedRoleConsultant);
    }


    @Override
    @Transactional
    public ManageRoleConsultantDTO updateRoleConsultant(Integer id, Integer roleId, RoleConsultantRequest roleConsultantRequest) {
        RoleConsultantEntity existingRoleConsultant = roleConsultantRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy role consultant với ID: " + id));

        RoleEntity role = roleRepository.findById(roleId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với ID: " + roleId));

        existingRoleConsultant.setName(roleConsultantRequest.getName());
        existingRoleConsultant.setRole(role);
        RoleConsultantEntity updatedRoleConsultant = roleConsultantRepository.save(existingRoleConsultant);
        return roleConsultantMapper.mapToDTO(updatedRoleConsultant);
    }

    @Override
    @Transactional
    public void deleteRoleConsultantById(Integer id) {
        RoleConsultantEntity roleConsultant = roleConsultantRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy role consultant với ID: " + id));
        roleConsultantRepository.delete(roleConsultant);
    }

    @Override
    public ManageRoleConsultantDTO getRoleConsultantById(Integer id) {
        return roleConsultantRepository.findById(id)
                .map(roleConsultantMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy role consultant với ID: " + id));
    }

    @Override
    public Page<ManageRoleConsultantDTO> getAllRoleConsultantsWithFilters(String name, Optional<Integer> roleId, Pageable pageable) {
        Specification<RoleConsultantEntity> spec = Specification.where(null);

        if (name != null && name.isEmpty()) {
            spec = spec.and(RoleConsultantSpecification.hasName(name));
        }

        if (roleId.isPresent()) {
            spec = spec.and(RoleConsultantSpecification.hasRoleId(roleId.get()));
        }

        return roleConsultantRepository.findAll(spec, pageable)
                .map(roleConsultantMapper::mapToDTO);
    }

    @Override
    public boolean existsById(Integer id) {
        return roleConsultantRepository.existsById(id);
    }

    @Override
    @Transactional
    public void importRoleConsultants(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ManageRoleConsultantDTO> roleConsultants = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        String name = row.get(1);
                        Integer roleId = Integer.parseInt(row.get(2));
                        LocalDate createdAt = LocalDate.parse(row.get(3));

                        return ManageRoleConsultantDTO.builder()
                                .id(id)
                                .name(name)
                                .roleId(roleId)
                                .createdAt(createdAt)
                                .build();
                    } catch (Exception e) {
                        throw new ErrorException("Lỗi khi parse dữ liệu Role Consultant: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        roleConsultants.forEach(roleConsultantDTO -> {
            try {
                RoleConsultantEntity entity = new RoleConsultantEntity();
                entity.setId(roleConsultantDTO.getId());
                entity.setName(roleConsultantDTO.getName());
                entity.setCreatedAt(roleConsultantDTO.getCreatedAt());

                RoleEntity role = roleRepository.findById(roleConsultantDTO.getRoleId())
                        .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với ID: " + roleConsultantDTO.getRoleId()));
                entity.setRole(role);

                roleConsultantRepository.save(entity);
            } catch (Exception e) {
                throw new ErrorException("Lỗi khi lưu Role Consultant vào database: " + e.getMessage());
            }
        });
    }

}
