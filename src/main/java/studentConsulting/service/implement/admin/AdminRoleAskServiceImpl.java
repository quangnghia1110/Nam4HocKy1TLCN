package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.RoleAskEntity;
import studentConsulting.model.entity.RoleEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.manage.ManageRoleAskDTO;
import studentConsulting.model.payload.mapper.admin.RoleAskMapper;
import studentConsulting.model.payload.request.RoleAskRequest;
import studentConsulting.repository.admin.RoleAskRepository;
import studentConsulting.repository.admin.RoleRepository;
import studentConsulting.service.interfaces.admin.IAdminRoleAskService;
import studentConsulting.specification.admin.RoleAskSpecification;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdminRoleAskServiceImpl implements IAdminRoleAskService {

    @Autowired
    private RoleAskRepository roleAskRepository;

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    private RoleAskMapper roleAskMapper;

    @Override
    @Transactional
    public ManageRoleAskDTO createRoleAsk(Integer roleId, RoleAskRequest roleAskRequest) {
        RoleEntity role = roleRepository.findById(roleId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với ID: " + roleId));

        RoleAskEntity roleAsk = RoleAskEntity.builder()
                .name(roleAskRequest.getName())
                .role(role)
                .createdAt(LocalDate.now())
                .build();

        RoleAskEntity savedRoleAsk = roleAskRepository.save(roleAsk);

        return roleAskMapper.mapToDTO(savedRoleAsk);
    }


    @Override
    @Transactional
    public ManageRoleAskDTO updateRoleAsk(Integer id, Integer roleId, RoleAskRequest roleAskRequest) {
        RoleAskEntity existingRoleAsk = roleAskRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy role ask với ID: " + id));

        RoleEntity role = roleRepository.findById(roleId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với ID: " + roleId));

        existingRoleAsk.setName(roleAskRequest.getName());
        existingRoleAsk.setRole(role);
        RoleAskEntity updatedRoleAsk = roleAskRepository.save(existingRoleAsk);
        return roleAskMapper.mapToDTO(updatedRoleAsk);
    }

    @Override
    @Transactional
    public void deleteRoleAskById(Integer id) {
        RoleAskEntity roleAsk = roleAskRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy role ask với ID: " + id));
        roleAskRepository.delete(roleAsk);
    }

    @Override
    public ManageRoleAskDTO getRoleAskById(Integer id) {
        return roleAskRepository.findById(id)
                .map(roleAskMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy role ask với ID: " + id));
    }

    @Override
    public Page<ManageRoleAskDTO> getAllRoleAsksWithFilters(String name, Optional<Integer> roleId, Pageable pageable) {
        Specification<RoleAskEntity> spec = Specification.where(null);

        if (name != null && !name.isEmpty()) {
            spec = spec.and(RoleAskSpecification.hasName(name));
        }

        if (roleId.isPresent()) {
            spec = spec.and(RoleAskSpecification.hasRoleId(roleId.get()));
        }

        return roleAskRepository.findAll(spec, pageable)
                .map(roleAskMapper::mapToDTO);
    }

    @Override
    public boolean existsById(Integer id) {
        return roleAskRepository.existsById(id);
    }

    @Override
    public void importRoleAsks(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        List<ManageRoleAskDTO> roleAsks = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        String name = row.get(1);
                        LocalDate createdAt = LocalDate.parse(row.get(2), dateFormatter);
                        Integer roleId = Integer.parseInt(row.get(3));

                        return ManageRoleAskDTO.builder()
                                .id(id)
                                .name(name)
                                .createdAt(createdAt)
                                .roleId(roleId)
                                .build();
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Role Ask: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        roleAsks.forEach(roleAsk -> {
            try {
                RoleAskEntity entity = new RoleAskEntity();
                entity.setId(roleAsk.getId());
                entity.setName(roleAsk.getName());
                entity.setCreatedAt(roleAsk.getCreatedAt());

                RoleEntity role = roleRepository.findById(roleAsk.getRoleId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy vai trò với ID: " + roleAsk.getRoleId()));
                entity.setRole(role);

                roleAskRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Role Ask vào database: " + e.getMessage());
            }
        });
    }


}
