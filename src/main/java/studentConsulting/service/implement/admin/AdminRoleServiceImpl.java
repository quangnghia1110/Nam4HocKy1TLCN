package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.RoleEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.RoleDTO;
import studentConsulting.model.payload.mapper.admin.RoleMapper;
import studentConsulting.model.payload.request.RoleRequest;
import studentConsulting.repository.admin.RoleRepository;
import studentConsulting.service.interfaces.admin.IAdminRoleService;

import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class AdminRoleServiceImpl implements IAdminRoleService {

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    private RoleMapper roleMapper;

    @Override
    @Transactional
    public RoleDTO createRole(RoleRequest roleRequest) {
        RoleEntity role = RoleEntity.builder()
                .name(roleRequest.getName())
                .createdAt(LocalDate.now())
                .build();

        RoleEntity savedRole = roleRepository.save(role);

        return roleMapper.mapToDTO(savedRole);
    }

    @Override
    @Transactional
    public RoleDTO updateRole(Integer id, RoleRequest roleRequest) {
        RoleEntity existingRole = roleRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với ID: " + id));

        existingRole.setName(roleRequest.getName());
        existingRole.setCreatedAt(LocalDate.now());
        RoleEntity updatedRole = roleRepository.save(existingRole);
        return roleMapper.mapToDTO(updatedRole);
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
                .map(roleMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với ID: " + id));
    }

    @Override
    public Page<RoleDTO> getAllRolesWithFilters(String name, Pageable pageable) {
        return roleRepository.findAllByNameContaining((name != null) ? name : "", pageable)
                .map(roleMapper::mapToDTO);
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
                        LocalDate createdAt = LocalDate.parse(row.get(3));
                        return new RoleDTO(id, name, createdAt);
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
                entity.setCreatedAt(role.getCreatedAt());
                roleRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Role vào database: " + e.getMessage());
            }
        });
    }

}

