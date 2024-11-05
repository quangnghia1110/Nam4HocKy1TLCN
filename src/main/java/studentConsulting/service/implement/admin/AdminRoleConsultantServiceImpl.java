package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.RoleConsultantEntity;
import studentConsulting.model.entity.RoleEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.manage.ManageRoleConsultantDTO;
import studentConsulting.model.payload.mapper.admin.RoleConsultantMapper;
import studentConsulting.model.payload.request.RoleConsultantRequest;
import studentConsulting.repository.admin.RoleConsultantRepository;
import studentConsulting.repository.admin.RoleRepository;
import studentConsulting.service.interfaces.admin.IAdminRoleConsultantService;
import studentConsulting.specification.admin.RoleConsultantSpecification;

import java.time.LocalDate;
import java.util.Optional;

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
    public Page<ManageRoleConsultantDTO> getRoleConsultantByAdmin(String name, Optional<Integer> roleId, Pageable pageable) {
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

}
