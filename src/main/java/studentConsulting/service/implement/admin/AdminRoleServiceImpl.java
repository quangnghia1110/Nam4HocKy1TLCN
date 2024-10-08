package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.authentication.RoleEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.authentication.RoleDTO;
import studentConsulting.model.payload.request.authentication.RoleRequest;
import studentConsulting.repository.authentication.RoleRepository;
import studentConsulting.service.interfaces.admin.IAdminRoleService;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdminRoleServiceImpl implements IAdminRoleService {

    @Autowired
    private RoleRepository roleRepository;

    private RoleDTO mapToDTO(RoleEntity role) {
        return RoleDTO.builder()
                .id(role.getId())
                .name(role.getName())
                .build();
    }

    private RoleEntity mapToEntity(RoleRequest roleRequest) {
        return RoleEntity.builder()
                .name(roleRequest.getName())
                .build();
    }

    @Override
    @Transactional
    public RoleDTO createRole(RoleRequest roleRequest) {
        RoleEntity role = mapToEntity(roleRequest);
        RoleEntity savedRole = roleRepository.save(role);
        return mapToDTO(savedRole);
    }

    @Override
    @Transactional
    public RoleDTO updateRole(Integer id, RoleRequest roleRequest) {
        RoleEntity existingRole = roleRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với ID: " + id));

        existingRole.setName(roleRequest.getName());
        RoleEntity updatedRole = roleRepository.save(existingRole);
        return mapToDTO(updatedRole);
    }

    @Override
    @Transactional
    public void deleteRoleById(Integer id) {
        RoleEntity role = roleRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với ID: " + id));
        roleRepository.delete(role);
    }

    @Override
    public RoleDTO getRoleById(Integer id) {
        return roleRepository.findById(id)
                .map(this::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với ID: " + id));
    }

    @Override
    public Page<RoleDTO> getAllRolesWithFilters(Optional<String> name, Pageable pageable) {
        return roleRepository.findAllByNameContaining(name.orElse(""), pageable)
                .map(this::mapToDTO);
    }

    @Override
    public boolean existsById(Integer id) {
        return roleRepository.existsById(id);
    }

    @Override
    public void importRoles(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<RoleDTO> roles = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        String name = row.get(1);

                        return new RoleDTO(id, name);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Role: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        roles.forEach(role -> {
            try {
                RoleEntity entity = new RoleEntity();
                entity.setId(role.getId());
                entity.setName(role.getName());

                roleRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Role vào database: " + e.getMessage());
            }
        });
    }

}

