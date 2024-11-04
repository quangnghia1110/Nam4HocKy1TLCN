package studentConsulting.service.implement.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.*;
import studentConsulting.model.payload.dto.common.StatisticAdminDTO;
import studentConsulting.repository.admin.*;
import studentConsulting.service.interfaces.common.IStatisticAdminService;
import studentConsulting.specification.admin.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
public class StatisticAdminServiceImpl implements IStatisticAdminService {

    @Autowired
    private AccountRepository accountRepository;

    @Autowired
    private AddressRepository addressRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private DistrictRepository districtRepository;

    @Autowired
    private FieldRepository fieldRepository;

    @Autowired
    private ProvinceRepository provinceRepository;

    @Autowired
    private RoleAskRepository roleAskRepository;

    @Autowired
    private RoleConsultantRepository roleConsultantRepository;

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    private WardRepository wardRepository;

    @Override
    public StatisticAdminDTO getAllAdminStatistics() {
        StatisticAdminDTO statistics = new StatisticAdminDTO();

        statistics.setTotalAccounts(accountRepository.count());
        statistics.setTotalAddresses(addressRepository.count());
        statistics.setTotalDepartments(departmentRepository.count());
        statistics.setTotalDistricts(districtRepository.count());
        statistics.setTotalFields(fieldRepository.count());
        statistics.setTotalProvinces(provinceRepository.count());
        statistics.setTotalRoleAsks(roleAskRepository.count());
        statistics.setTotalRoleConsultants(roleConsultantRepository.count());
        statistics.setTotalRoles(roleRepository.count());
        statistics.setTotalWards(wardRepository.count());

        return statistics;
    }

    @Override
    public List<Map<String, Object>> getAccountsByYear(Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<AccountEntity> spec = Specification.where(AccountSpecification.hasExactYear(year));

        List<AccountEntity> accountEntities = accountRepository.findAll(spec);

        for (AccountEntity account : accountEntities) {
            int month = account.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getDepartmentsByYear(Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<DepartmentEntity> spec = Specification.where(DepartmentSpecification.hasExactYear(year));

        List<DepartmentEntity> departmentEntities = departmentRepository.findAll(spec);

        for (DepartmentEntity department : departmentEntities) {
            int month = department.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getFieldsByYear(Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<FieldEntity> spec = Specification.where(FieldSpecification.hasExactYear(year));

        if (departmentId != null) {
            spec = spec.and(FieldSpecification.hasDepartment(departmentId));
        }

        List<FieldEntity> fieldEntities = fieldRepository.findAll(spec);

        for (FieldEntity field : fieldEntities) {
            int month = field.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getRoleAsksByYear(Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<RoleAskEntity> spec = Specification.where(RoleAskSpecification.hasExactYear(year));

        List<RoleAskEntity> roleAskEntities = roleAskRepository.findAll(spec);

        for (RoleAskEntity roleAsk : roleAskEntities) {
            int month = roleAsk.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getRoleConsultantsByYear(Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<RoleConsultantEntity> spec = Specification.where(RoleConsultantSpecification.hasExactYear(year));

        List<RoleConsultantEntity> roleConsultantEntities = roleConsultantRepository.findAll(spec);

        for (RoleConsultantEntity roleConsultant : roleConsultantEntities) {
            int month = roleConsultant.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getRolesByYear(Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<RoleEntity> spec = Specification.where(RoleSpecification.hasExactYear(year));

        List<RoleEntity> roleEntities = roleRepository.findAll(spec);

        for (RoleEntity role : roleEntities) {
            int month = role.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

//    @Override
//    public List<Map<String, Object>> getAddressesByYear(Integer year) {
//        Map<Integer, Long> monthlyCount = new HashMap<>();
//        for (int i = 1; i <= 12; i++) {
//            monthlyCount.put(i, 0L);
//        }
//
//        Specification<AddressEntity> spec = Specification.where(AddressSpecification.hasExactYear(year));
//
//        List<AddressEntity> addressEntities = addressRepository.findAll(spec);
//
//        for (AddressEntity address : addressEntities) {
//            int month = address.getCreatedAt().getMonthValue();
//            monthlyCount.put(month, monthlyCount.get(month) + 1);
//        }
//
//        return monthlyCount.entrySet().stream()
//                .map(entry -> Map.<String, Object>of(
//                        "year", year,
//                        "month", entry.getKey(),
//                        "count", entry.getValue()))
//                .collect(Collectors.toList());
//    }
//
//    @Override
//    public List<Map<String, Object>> getProvincesByYear(Integer year) {
//        Map<Integer, Long> monthlyCount = new HashMap<>();
//        for (int i = 1; i <= 12; i++) {
//            monthlyCount.put(i, 0L);
//        }
//
//        Specification<ProvinceEntity> spec = Specification.where(ProvinceSpecification.hasExactYear(year));
//
//        List<ProvinceEntity> provinceEntities = provinceRepository.findAll(spec);
//
//        for (ProvinceEntity province : provinceEntities) {
//            int month = province.getCreatedAt().getMonthValue();
//            monthlyCount.put(month, monthlyCount.get(month) + 1);
//        }
//
//        return monthlyCount.entrySet().stream()
//                .map(entry -> Map.<String, Object>of(
//                        "year", year,
//                        "month", entry.getKey(),
//                        "count", entry.getValue()))
//                .collect(Collectors.toList());
//    }
//
//    @Override
//    public List<Map<String, Object>> getDistrictsByYear(Integer year) {
//        Map<Integer, Long> monthlyCount = new HashMap<>();
//        for (int i = 1; i <= 12; i++) {
//            monthlyCount.put(i, 0L);
//        }
//
//        Specification<DistrictEntity> spec = Specification.where(DistrictSpecification.hasExactYear(year));
//
//        List<DistrictEntity> districtEntities = districtRepository.findAll(spec);
//
//        for (DistrictEntity district : districtEntities) {
//            int month = district.getCreatedAt().getMonthValue();
//            monthlyCount.put(month, monthlyCount.get(month) + 1);
//        }
//
//        return monthlyCount.entrySet().stream()
//                .map(entry -> Map.<String, Object>of(
//                        "year", year,
//                        "month", entry.getKey(),
//                        "count", entry.getValue()))
//                .collect(Collectors.toList());
//    }
//
//    @Override
//    public List<Map<String, Object>> getWardsByYear(Integer year) {
//        Map<Integer, Long> monthlyCount = new HashMap<>();
//        for (int i = 1; i <= 12; i++) {
//            monthlyCount.put(i, 0L);
//        }
//
//        Specification<WardEntity> spec = Specification.where(WardSpecification.hasExactYear(year));
//
//        List<WardEntity> wardEntities = wardRepository.findAll(spec);
//
//        for (WardEntity ward : wardEntities) {
//            int month = ward.getCreatedAt().getMonthValue();
//            monthlyCount.put(month, monthlyCount.get(month) + 1);
//        }
//
//        return monthlyCount.entrySet().stream()
//                .map(entry -> Map.<String, Object>of(
//                        "year", year,
//                        "month", entry.getKey(),
//                        "count", entry.getValue()))
//                .collect(Collectors.toList());
//    }
}
