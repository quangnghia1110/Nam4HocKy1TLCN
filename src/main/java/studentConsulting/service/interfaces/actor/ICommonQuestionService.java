package studentConsulting.service.interfaces.actor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.CommonQuestionDTO;
import studentConsulting.model.payload.request.CommonQuestionRequest;

import java.security.Principal;
import java.time.LocalDate;

public interface ICommonQuestionService {

    CommonQuestionDTO convertToCommonQuestion(Integer questionId, Principal principal);

    CommonQuestionDTO updateCommonQuestion(Integer commonQuestionId, CommonQuestionRequest request, Principal principal);

    void deleteCommonQuestion(Integer id, UserInformationEntity user);

    CommonQuestionDTO getCommonQuestionById(Integer questionId, UserInformationEntity user);

    Page<CommonQuestionDTO> getCommonQuestionByRole(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable);

    CommonQuestionDTO createCommonQuestion(CommonQuestionRequest request, MultipartFile file, Principal principal);

    }
