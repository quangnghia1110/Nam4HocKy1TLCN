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
import studentConsulting.model.payload.request.authentication.RoleConsultantRequest;
import studentConsulting.repository.authentication.RoleRepository;
import studentConsulting.repository.user.RoleConsultantRepository;
import studentConsulting.service.interfaces.admin.IAdminRoleConsultantService;
import studentConsulting.specification.authentication.RoleConsultantSpecification;

import java.time.LocalDate;
import java.util.Optional;

@Service
public class AdminRoleConsultantServiceImpl implements IAdminRoleConsultantService {

    @Autowired
    private RoleConsultantRepository roleConsultantRepository;

    @Autowired
    private RoleRepository roleRepository;

    private ManageRoleConsultantDTO mapToDTO(RoleConsultantEntity roleConsultant) {
        return ManageRoleConsultantDTO.builder()
                .id(roleConsultant.getId())
                .createdAt(roleConsultant.getCreatedAt())
                .name(roleConsultant.getName())
                .roleId(roleConsultant.getRole().getId())
                .build();
    }

    private RoleConsultantEntity mapToEntity(RoleConsultantRequest roleConsultantRequest, RoleEntity role) {
        return RoleConsultantEntity.builder()
                .name(roleConsultantRequest.getName())
                .role(role)
                .createdAt(LocalDate.now())
                .build();
    }

    @Override
    @Transactional
    public ManageRoleConsultantDTO createRoleConsultant(Integer roleId, RoleConsultantRequest roleConsultantRequest) {
        RoleEntity role = roleRepository.findById(roleId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với ID: " + roleId));

        RoleConsultantEntity roleConsultant = mapToEntity(roleConsultantRequest, role);
        RoleConsultantEntity savedRoleConsultant = roleConsultantRepository.save(roleConsultant);
        return mapToDTO(savedRoleConsultant);
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
        return mapToDTO(updatedRoleConsultant);
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
                .map(this::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy role consultant với ID: " + id));
    }

    @Override
    public Page<ManageRoleConsultantDTO> getAllRoleConsultantsWithFilters(Optional<String> name, Optional<Integer> roleId, Pageable pageable) {
        Specification<RoleConsultantEntity> spec = Specification.where(null);

        if (name.isPresent()) {
            spec = spec.and(RoleConsultantSpecification.hasName(name.get()));
        }

        if (roleId.isPresent()) {
            spec = spec.and(RoleConsultantSpecification.hasRoleId(roleId.get()));
        }

        return roleConsultantRepository.findAll(spec, pageable)
                .map(this::mapToDTO);
    }

    @Override
    public boolean existsById(Integer id) {
        return roleConsultantRepository.existsById(id);
    }
}
