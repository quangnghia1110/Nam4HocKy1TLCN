package studentConsulting.service.implement.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.question_answer.ForwardQuestionEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO;
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO.ConsultantDTO;
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO.DepartmentDTO;
import studentConsulting.model.payload.request.question_answer.UpdateForwardQuestionRequest;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.question_answer.ForwardQuestionRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorForwardQuestionService;
import studentConsulting.specification.question_answer.ForwardQuestionSpecification;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdvisorForwardQuestionServiceImpl implements IAdvisorForwardQuestionService {

    @Autowired
    private ForwardQuestionRepository forwardQuestionRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private UserRepository consultantRepository;

    @Autowired
    private UserRepository userRepository;

    @Override
    public Page<ForwardQuestionDTO> getForwardQuestionByRole(String title, Integer toDepartmentId, LocalDate startDate, LocalDate endDate, Pageable pageable, Integer userId, Integer departmentId, boolean isAdmin, boolean isAdvisor) {

        Specification<ForwardQuestionEntity> spec = Specification.where(null);
        if(!isAdmin) {
            if (isAdvisor) {
                spec = spec.and(
                        ForwardQuestionSpecification.hasFromDepartment(departmentId)
                                .or(ForwardQuestionSpecification.hasToDepartment(departmentId))
                                .or(ForwardQuestionSpecification.hasCreatedBy(userId))
                );
            } else {
                spec = spec.and(
                        ForwardQuestionSpecification.hasCreatedBy(userId)
                                .or(ForwardQuestionSpecification.hasToDepartment(toDepartmentId))
                );
            }
        }
        if (title != null) {
            spec = spec.and(ForwardQuestionSpecification.hasTitle(title));
        }
        if (startDate != null && endDate != null) {
            spec = spec.and(ForwardQuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ForwardQuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ForwardQuestionSpecification.hasDateBefore(endDate));
        }

        Page<ForwardQuestionEntity> forwardQuestions = forwardQuestionRepository.findAll(spec, pageable);

        return forwardQuestions.map(forwardQuestion -> mapToForwardQuestionDTO(forwardQuestion, forwardQuestion.getConsultant().getId()));
    }


    @Override
    public ForwardQuestionDTO updateForwardQuestionByRole(Integer forwardQuestionId, UpdateForwardQuestionRequest forwardQuestionRequest, Integer userId, Integer departmentId, boolean isAdmin, boolean isAdvisor) {

        ForwardQuestionEntity forwardQuestion = forwardQuestionRepository.findById(forwardQuestionId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy câu hỏi chuyển tiếp"));

        if (!isAdmin) {
            if (isAdvisor) {
                if (!forwardQuestion.getFromDepartment().getId().equals(departmentId)
                        && !forwardQuestion.getToDepartment().getId().equals(departmentId)
                        && !forwardQuestion.getCreatedBy().getId().equals(userId)) {
                    throw new ErrorException("Bạn không có quyền cập nhật câu hỏi này.");
                }
            } else {
                if (!forwardQuestion.getCreatedBy().getId().equals(userId)) {
                    throw new ErrorException("Bạn chỉ có thể cập nhật câu hỏi do bạn tạo.");
                }
            }
        }

        forwardQuestion.setTitle(forwardQuestionRequest.getTitle());
        DepartmentEntity toDepartment = departmentRepository.findById(forwardQuestionRequest.getToDepartmentId())
                .orElseThrow(() -> new ErrorException("Phòng ban không tồn tại"));
        forwardQuestion.setToDepartment(toDepartment);

        QuestionEntity question = questionRepository.findById(forwardQuestionRequest.getQuestionId())
                .orElseThrow(() -> new ErrorException("Câu hỏi không tồn tại"));
        forwardQuestion.setQuestion(question);

        ForwardQuestionEntity updatedForwardQuestion = forwardQuestionRepository.save(forwardQuestion);
        return mapToForwardQuestionDTO(updatedForwardQuestion, forwardQuestionRequest.getConsultantId());
    }


    @Override
    @Transactional
    public void deleteForwardQuestionByRole(Integer forwardQuestionId, Integer userId, Integer departmentId, boolean isAdmin, boolean isAdvisor) {

        ForwardQuestionEntity forwardQuestion = forwardQuestionRepository.findById(forwardQuestionId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy câu hỏi chuyển tiếp"));

        if (!isAdmin) {
            if (isAdvisor) {
                if (!forwardQuestion.getFromDepartment().getId().equals(departmentId)
                        && !forwardQuestion.getToDepartment().getId().equals(departmentId)
                        && !forwardQuestion.getCreatedBy().getId().equals(userId)) {
                    throw new ErrorException("Bạn không có quyền xóa câu hỏi này.");
                }
            } else {
                if (!forwardQuestion.getCreatedBy().getId().equals(userId)) {
                    throw new ErrorException("Bạn chỉ có thể xóa câu hỏi do bạn tạo.");
                }
            }
        }

        forwardQuestionRepository.delete(forwardQuestion);
    }


    @Override
    public ForwardQuestionDTO getForwardQuestionDetailByRole(Integer forwardQuestionId, Integer userId, Integer departmentId, boolean isAdmin, boolean isAdvisor) {
        Specification<ForwardQuestionEntity> spec;

        if (isAdmin) {
            spec = Specification.where(ForwardQuestionSpecification.hasId(forwardQuestionId));
        } else if (isAdvisor) {
            spec = Specification.where(ForwardQuestionSpecification.hasId(forwardQuestionId))
                    .and(ForwardQuestionSpecification.isFromOrToDepartment(departmentId));
        } else {
            spec = Specification.where(ForwardQuestionSpecification.hasId(forwardQuestionId))
                    .and(ForwardQuestionSpecification.hasCreatedBy(userId));
        }

        return forwardQuestionRepository.findOne(spec)
                .map(forwardQuestion -> mapToForwardQuestionDTO(forwardQuestion, forwardQuestion.getConsultant().getId())) // Sử dụng lambda thay vì method reference
                .orElse(null);
    }



    private ForwardQuestionDTO mapToForwardQuestionDTO(ForwardQuestionEntity forwardQuestion, Integer consultantId) {
        ForwardQuestionDTO.DepartmentDTO fromDepartmentDTO = ForwardQuestionDTO.DepartmentDTO.builder()
                .id(forwardQuestion.getFromDepartment().getId())
                .name(forwardQuestion.getFromDepartment().getName())
                .build();

        ForwardQuestionDTO.DepartmentDTO toDepartmentDTO = ForwardQuestionDTO.DepartmentDTO.builder()
                .id(forwardQuestion.getToDepartment().getId())
                .name(forwardQuestion.getToDepartment().getName())
                .build();

        ForwardQuestionDTO.ConsultantDTO consultantDTO = ForwardQuestionDTO.ConsultantDTO.builder()
                .id(consultantId)
                .firstName(forwardQuestion.getConsultant().getFirstName())
                .lastName(forwardQuestion.getConsultant().getLastName())
                .build();

        Integer createdBy = (forwardQuestion.getCreatedBy() != null) ? forwardQuestion.getCreatedBy().getId() : null;

        return ForwardQuestionDTO.builder()
                .id(forwardQuestion.getId())
                .title(forwardQuestion.getTitle())
                .fromDepartment(fromDepartmentDTO)
                .toDepartment(toDepartmentDTO)
                .consultant(consultantDTO)
                .statusForward(forwardQuestion.getStatusForward())
                .createdBy(createdBy)
                .build();
    }

    @Override
    public void importForwardQuestions(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ForwardQuestionDTO> forwardQuestions = filteredData.stream()
                .map(row -> {
                    try {
                        Integer forwardQuestionId = Integer.parseInt(row.get(0));
                        Integer fromDepartmentId = Integer.parseInt(row.get(1));
                        Integer toDepartmentId = Integer.parseInt(row.get(2));
                        Integer consultantId = Integer.parseInt(row.get(3));
                        Integer createdById = Integer.parseInt(row.get(4));
                        Boolean statusForward = Boolean.parseBoolean(row.get(5));
                        LocalDate createdAt = LocalDate.parse(row.get(6));

                        return ForwardQuestionDTO.builder()
                                .id(forwardQuestionId)
                                .fromDepartment(new DepartmentDTO(fromDepartmentId, null))
                                .toDepartment(new DepartmentDTO(toDepartmentId, null))
                                .consultant(new ConsultantDTO(consultantId, null, null))
                                .createdBy(createdById)
                                .statusForward(statusForward)
                                .build();
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Forward Question: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        forwardQuestions.forEach(question -> {
            try {
                ForwardQuestionEntity entity = new ForwardQuestionEntity();

                entity.setFromDepartment(departmentRepository.findById(question.getFromDepartment().getId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy phòng ban với ID: " + question.getFromDepartment().getId())));
                entity.setToDepartment(departmentRepository.findById(question.getToDepartment().getId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy phòng ban với ID: " + question.getToDepartment().getId())));
                entity.setConsultant(consultantRepository.findById(question.getConsultant().getId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy tư vấn viên với ID: " + question.getConsultant().getId())));
                entity.setCreatedBy(userRepository.findById(question.getCreatedBy())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy người tạo với ID: " + question.getCreatedBy())));
                entity.setStatusForward(question.getStatusForward());

                forwardQuestionRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Forward Question vào database: " + e.getMessage());
            }
        });
    }


}
