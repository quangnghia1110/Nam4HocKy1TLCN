package studentConsulting.service.implement.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.AnswerEntity;
import studentConsulting.model.entity.CommonQuestionEntity;
import studentConsulting.model.entity.QuestionEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.CommonQuestionDTO;
import studentConsulting.model.payload.mapper.actor.CommonQuestionMapper;
import studentConsulting.model.payload.request.UpdateCommonQuestionRequest;
import studentConsulting.repository.actor.AnswerRepository;
import studentConsulting.repository.actor.CommonQuestionRepository;
import studentConsulting.repository.actor.QuestionRepository;
import studentConsulting.repository.admin.DepartmentRepository;
import studentConsulting.repository.admin.FieldRepository;
import studentConsulting.repository.admin.RoleAskRepository;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.implement.common.FileStorageServiceImpl;
import studentConsulting.service.interfaces.actor.ICommonQuestionService;
import studentConsulting.specification.actor.CommonQuestionSpecification;

import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class CommonQuestionServiceImpl implements ICommonQuestionService {

    @Autowired
    private CommonQuestionRepository commonQuestionRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private FieldRepository fieldRepository;

    @Autowired
    private RoleAskRepository roleAskRepository;

    @Autowired
    private FileStorageServiceImpl fileStorageService;

    @Autowired
    private CommonQuestionMapper commonQuestionMapper;

    @Override
    public Page<CommonQuestionDTO> getCommonQuestionsWithFilters(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<CommonQuestionEntity> spec = Specification.where(null);

        if (departmentId != null) {
            spec = spec.and(CommonQuestionSpecification.isCreatedByAdvisor(departmentId));
        }

        if (title != null && !title.isEmpty()) {
            spec = spec.and(CommonQuestionSpecification.hasTitle(title));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasDateBefore(endDate));
        }

        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findAll(spec, pageable);
        return commonQuestions.map(commonQuestionMapper::mapToDTO);
    }

    @Override
    @Transactional
    public CommonQuestionDTO convertToCommonQuestion(Integer questionId, Principal principal) {
        String email = principal.getName();
        UserInformationEntity createdByUser = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        QuestionEntity question = questionRepository.findById(questionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

        return createCommonQuestionFromQuestionEntity(question, createdByUser);
    }

    @Override
    @Transactional
    public CommonQuestionDTO convertToCommonQuestionByDepartment(Integer questionId, Integer departmentId, Principal principal) {
        QuestionEntity question = questionRepository.findById(questionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));

        if (!question.getDepartment().getId().equals(departmentId)) {
            throw new ErrorException("Câu hỏi không thuộc phòng ban của bạn");
        }

        String email = principal.getName();
        UserInformationEntity createdByUser = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        return createCommonQuestionFromQuestionEntity(question, createdByUser);
    }

    private CommonQuestionDTO createCommonQuestionFromQuestionEntity(QuestionEntity question, UserInformationEntity createdByUser) {
        CommonQuestionEntity commonQuestion = new CommonQuestionEntity();
        commonQuestion.setTitle(question.getTitle());
        commonQuestion.setContent(question.getContent());
        commonQuestion.setFileName(question.getFileName());
        commonQuestion.setCreatedAt(question.getCreatedAt());
        commonQuestion.setViews(question.getViews());
        commonQuestion.setDepartment(question.getDepartment());
        commonQuestion.setField(question.getField());
        commonQuestion.setRoleAsk(question.getRoleAsk());
        commonQuestion.setUser(question.getUser());
        commonQuestion.setCreatedBy(createdByUser);

        Optional<AnswerEntity> firstAnswer = answerRepository.findFirstAnswerByQuestionId(question.getId());
        if (firstAnswer.isPresent()) {
            AnswerEntity answer = firstAnswer.get();
            commonQuestion.setAnswerContent(answer.getContent());
            commonQuestion.setAnswerTitle(answer.getTitle());
            commonQuestion.setAnswerCreatedAt(answer.getCreatedAt());
        }

        CommonQuestionEntity savedCommonQuestion = commonQuestionRepository.save(commonQuestion);

        return commonQuestionMapper.mapToDTO(savedCommonQuestion);
    }

    @Override
    @Transactional
    public CommonQuestionDTO updateCommonQuestion(Integer commonQuestionId, UpdateCommonQuestionRequest request) {
        CommonQuestionEntity existingCommonQuestion = commonQuestionRepository.findById(commonQuestionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi tổng hợp không tồn tại"));

        return updateCommonQuestionEntity(existingCommonQuestion, request);
    }

    @Override
    @Transactional
    public CommonQuestionDTO updateCommonQuestionByDepartment(Integer commonQuestionId, Integer departmentId, UpdateCommonQuestionRequest request) {
        CommonQuestionEntity existingCommonQuestion = commonQuestionRepository.findById(commonQuestionId)
                .orElseThrow(() -> new ErrorException("Câu hỏi tổng hợp không tồn tại"));

        if (!existingCommonQuestion.getDepartment().getId().equals(departmentId)) {
            throw new ErrorException("Bạn không có quyền chỉnh sửa câu hỏi này.");
        }

        return updateCommonQuestionEntity(existingCommonQuestion, request);
    }

    private CommonQuestionDTO updateCommonQuestionEntity(CommonQuestionEntity commonQuestion, UpdateCommonQuestionRequest request) {
        commonQuestion.setTitle(request.getTitle());
        commonQuestion.setContent(request.getContent());
        if (request.getFileName() != null && !request.getFileName().isEmpty()) {
            String fileName = fileStorageService.saveFile(request.getFileName());
            commonQuestion.setFileName(fileName);
        }
        commonQuestion.setAnswerTitle(request.getAnswerTitle());
        commonQuestion.setAnswerContent(request.getAnswerContent());

        CommonQuestionEntity updatedCommonQuestion = commonQuestionRepository.save(commonQuestion);

        return commonQuestionMapper.mapToDTO(updatedCommonQuestion);
    }

    @Override
    @Transactional
    public void deleteCommonQuestion(Integer id) {
        commonQuestionRepository.deleteById(id);
    }

    @Override
    @Transactional
    public void deleteCommonQuestionByDepartment(Integer id, Integer departmentId) {
        CommonQuestionEntity commonQuestion = commonQuestionRepository.findByIdAndDepartmentId(id, departmentId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy câu hỏi tổng hợp"));
        commonQuestionRepository.delete(commonQuestion);
    }

    @Override
    public CommonQuestionDTO getCommonQuestionById(Integer questionId) {
        return commonQuestionRepository.findById(questionId)
                .map(commonQuestionMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy câu hỏi"));
    }

    @Override
    public CommonQuestionDTO getCommonQuestionByIdAndDepartment(Integer questionId, Integer departmentId) {
        return commonQuestionRepository.findByIdAndDepartmentId(questionId, departmentId)
                .map(commonQuestionMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy câu hỏi"));
    }

    @Override
    public Page<CommonQuestionDTO> getAllCommonQuestionsWithFilters(String title, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<CommonQuestionEntity> spec = Specification.where(null);

        if (title != null && !title.isEmpty()) {
            spec = spec.and(CommonQuestionSpecification.hasTitle(title));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasDateBefore(endDate));
        }

        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findAll(spec, pageable);
        return commonQuestions.map(commonQuestionMapper::mapToDTO);
    }

    @Override
    public Page<CommonQuestionDTO> getCommonQuestionsWithAdvisorFilters(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<CommonQuestionEntity> spec = Specification.where(null);

        if (departmentId != null) {
            spec = spec.and(CommonQuestionSpecification.isCreatedByAdvisor(departmentId));
        }

        if (title != null && !title.isEmpty()) {
            spec = spec.and(CommonQuestionSpecification.hasTitle(title));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasDateBefore(endDate));
        }

        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findAll(spec, pageable);
        return commonQuestions.map(commonQuestionMapper::mapToDTO);
    }

    @Override
    public void importCommonQuestions(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<CommonQuestionDTO> commonQuestions = filteredData.stream()
                .map(row -> {
                    try {
                        String departmentName = row.get(0);
                        String fieldName = row.get(1);
                        String roleAskName = row.get(2);
                        String title = row.get(3);
                        String content = row.get(4);
                        String fileName = row.get(5);
                        Integer views = Integer.parseInt(row.get(6));
                        String askerFirstname = row.get(7);
                        String askerLastname = row.get(8);
                        String answerTitle = row.get(9);
                        String answerContent = row.get(10);
                        String answerUserEmail = row.get(11);
                        String answerUserFirstname = row.get(12);
                        String answerUserLastname = row.get(13);
                        LocalDate answerCreatedAt = LocalDate.parse(row.get(14));
                        String createdBy = row.get(15);

                        var department = departmentRepository.findByName(departmentName)
                                .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với tên: " + departmentName));

                        var field = fieldRepository.findByName(fieldName)
                                .orElseThrow(() -> new ErrorException("Không tìm thấy lĩnh vực với tên: " + fieldName));

                        var roleAsk = roleAskRepository.findByName(roleAskName)
                                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò với tên: " + roleAskName));

                        return CommonQuestionDTO.builder()
                                .department(CommonQuestionDTO.DepartmentDTO.builder()
                                        .id(department.getId())
                                        .name(departmentName)
                                        .build())
                                .field(CommonQuestionDTO.FieldDTO.builder()
                                        .id(field.getId())
                                        .name(fieldName)
                                        .build())
                                .roleAsk(CommonQuestionDTO.RoleAskDTO.builder()
                                        .id(roleAsk.getId())
                                        .name(roleAskName)
                                        .build())
                                .title(title)
                                .content(content)
                                .fileName(fileName)
                                .views(views)
                                .askerFirstname(askerFirstname)
                                .askerLastname(askerLastname)
                                .answerTitle(answerTitle)
                                .answerContent(answerContent)
                                .answerUserEmail(answerUserEmail)
                                .answerUserFirstname(answerUserFirstname)
                                .answerUserLastname(answerUserLastname)
                                .answerCreatedAt(answerCreatedAt)
                                .createdBy(createdBy)
                                .build();
                    } catch (Exception e) {
                        throw new ErrorException("Lỗi khi parse dữ liệu Common Question: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        commonQuestions.forEach(question -> {
            try {
                CommonQuestionEntity entity = new CommonQuestionEntity();
                entity.setDepartment(departmentRepository.findByName(question.getDepartment().getName()).get());
                entity.setField(fieldRepository.findByName(question.getField().getName()).get());
                entity.setRoleAsk(roleAskRepository.findByName(question.getRoleAsk().getName()).get());
                entity.setTitle(question.getTitle());
                entity.setContent(question.getContent());
                entity.setFileName(question.getFileName());
                entity.setViews(question.getViews());
                entity.setAskerFirstname(question.getAskerFirstname());
                entity.setAskerLastname(question.getAskerLastname());
                entity.setAnswerTitle(question.getAnswerTitle());
                entity.setAnswerContent(question.getAnswerContent());
                entity.setAnswerUserEmail(question.getAnswerUserEmail());
                entity.setAnswerUserFirstname(question.getAnswerUserFirstname());
                entity.setAnswerUserLastname(question.getAnswerUserLastname());
                entity.setAnswerCreatedAt(question.getAnswerCreatedAt());
                String id = question.getCreatedBy();

                UserInformationEntity createdBy = userRepository.findById(Integer.parseInt(id))
                        .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

                entity.setCreatedBy(createdBy);


                commonQuestionRepository.save(entity);
            } catch (Exception e) {
                throw new ErrorException("Lỗi khi lưu Common Question vào database: " + e.getMessage());
            }
        });
    }
}
